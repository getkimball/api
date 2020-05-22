-module(features_app).
-behaviour(application).
-include_lib("kernel/include/logger.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    ?LOG_INFO(#{what=><<"Starting">>}),
    App = features,
    Mode = get_features_mode(),

    ok = set_config(),

    Routes = [
        {"/metrics/[:registry]", prometheus_cowboy2_handler, []}
    ],
    StaticRoute = [
        {"/[...]", cowboy_static, {priv_dir, App, "public"}}
    ],

    Trails = setup_trails(),

    AllRoutes = Routes ++ Trails ++  StaticRoute,

    Dispatch = trails:single_host_compile(AllRoutes),

    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
      env => #{dispatch => Dispatch},
      metrics_callback => fun prometheus_cowboy2_instrumenter:observe/1,
      stream_handlers => [cowboy_metrics_h, cowboy_stream_h]
    }),
    features_sup:start_link(Mode).

stop(_State) ->
  ok.

setup_trails() ->
    Handlers = [
        features_handler_ok,
        features_handler_v0_features,
        cowboy_swagger_handler
    ],
    Trails = trails:trails(Handlers),
    trails:store(Trails),
    Trails.

set_config() ->
    setup_sentry(),
    setup_additional_namespace_config(),
    setup_file_store_path(),
    ok = application:set_env(trails, api_root, "/"),
    ok = application:set_env(cowboy_swagger, global_spec,
        #{swagger => "2.0",
          info => #{
            title => <<"Kimball Features API">>,
            version => <<"0.0.0">>
    }}),
    ok.

setup_additional_namespace_config() ->
    NamespacesString = os:getenv("ADDITIONAL_NAMESPACES", ""),
    NamespacesBin = binary:list_to_bin(NamespacesString),
    Namespaces = binary:split(NamespacesBin, <<",">>),
    Config = additional_namespaces_to_list(Namespaces),
    application:set_env(features, namespaces, Config),

    ?LOG_INFO(#{what=>"Sync to additional namespaces",
                namespaces=>Namespaces}),
    ok.

% Removes empty response from split
additional_namespaces_to_list([<<>>]) ->
    [];
additional_namespaces_to_list(List) ->
    List.

setup_sentry() ->
    DSN = os:getenv("SENTRY_DSN"),
    case DSN of
        false ->
            ?LOG_INFO(#{what=>"Sentry not setup. Set 'SENTRY_DSN'"});
        ActualDSN ->
            ?LOG_INFO(#{what=>"Sentry configured"}),
            ok = logger:add_handler(
                eraven,
                er_logger_handler,
                #{level => warning,
                  config => #{

                    dsn => ActualDSN
        }})
    end,
    ok.

setup_file_store_path() ->
    Name = file_store_path,
    case application:get_env(features, Name) of
        undefined -> application:set_env(features, Name, "/features/data");
        _ -> ok
    end.

get_features_mode() ->
    Mode = os:getenv("FEATURES_MODE"),
    % TODO: Do something more intelligent here to work in local dev
    case Mode of
        "sidecar" -> sidecar;
        "api" -> api_server;
        _ -> api_server
    end.
