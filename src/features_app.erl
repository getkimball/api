-module(features_app).
-behaviour(application).
-include_lib("kernel/include/logger.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    ?LOG_INFO(#{what=><<"Starting">>}),
    App = features,

    ok = set_config(),

    Routes = [
        {"/metrics/[:registry]", prometheus_cowboy2_handler, []}
    ],
    StaticRoute = [
        {"/[...]", cowboy_static, {priv_dir, App, "public"}}
    ],

    Handlers = [
        features_handler_ok,
        features_handler_v0_features,
        cowboy_swagger_handler
    ],
    Trails = trails:trails(Handlers),

    AllRoutes = Routes ++ Trails ++  StaticRoute,

    trails:store(Trails),
    Dispatch = trails:single_host_compile(AllRoutes),

    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
      env => #{dispatch => Dispatch},
      metrics_callback => fun prometheus_cowboy2_instrumenter:observe/1,
      stream_handlers => [cowboy_metrics_h, cowboy_stream_h]
    }),
    features_sup:start_link().

stop(_State) ->
  ok.

set_config() ->
    setup_sentry(),
    setup_additional_namespace_config(),
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
    application:set_env(features, namespaces, Namespaces),

    ?LOG_INFO(#{what=>"Sync to additional namespaces",
                namespaces=>Namespaces}),
    ok.

setup_sentry() ->
    DSN = os:getenv("SENTRY_DSN"),
    case DSN of
        false ->
            ?LOG_INFO(#{what=>"Sentry not setup. Set 'SENTRY_DSN'"});
        ActualDSN ->
            logger:add_handler(
                eraven,
                er_logger_handler,
                #{level => warning,
                  config => #{

                    dsn => ActualDSN
        }})
    end,
    ok.
