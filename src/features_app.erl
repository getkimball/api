-module(features_app).
-behaviour(application).
-include_lib("kernel/include/logger.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    {ok, VSN} = application:get_key(features, vsn),
    ?LOG_INFO(#{what=><<"Starting">>,
                version=>VSN}),
    App = features,
    Mode = get_features_mode(),

    ok = set_config(Mode),

    Routes = [
        {"/metrics/[:registry]", prometheus_cowboy2_handler, []}
    ],
    StaticRoute = [
        {"/", cowboy_static, {priv_file, App, "public/index.html"}},
        {"/[...]", cowboy_static, {priv_dir, App, "public"}}
    ],

    Trails = setup_trails(),

    AllRoutes = Routes ++ Trails ++  StaticRoute,

    Dispatch = trails:single_host_compile(AllRoutes),

    HTTPOpts = #{
      env => #{dispatch => Dispatch},
      metrics_callback => fun prometheus_cowboy2_instrumenter:observe/1,
      stream_handlers => [cowboy_metrics_h, cowboy_stream_h]
    },

    Port = list_to_integer(os:getenv("API_PORT", "8080")),

    {ok, _} = cowboy:start_clear(http, [{port, Port}], HTTPOpts),
    features_sup:start_link(Mode).

stop(_State) ->
  ok.

trails_handlers() ->
    Handlers = [
        features_handler_ok,
        features_handler_v0_analytics,
        features_handler_v0_features,
        features_handler_v0_feature_specs,
        cowboy_swagger_handler
    ],
    Handlers.

setup_trails() ->
    Handlers = trails_handlers(),
    Trails = trails:trails(Handlers),
    trails:store(Trails),
    Trails.

set_config(Mode) ->
    setup_sentry(),
    setup_namespace(),
    setup_additional_namespace_config(),
    setup_file_store_path(),
    setup_analytics_url(),
    setup_analytics_event_mod(Mode),

    ok = application:set_env(trails, api_root, "/"),
    ok = application:set_env(features, mode, Mode),
    ok = application:set_env(cowboy_swagger, global_spec,
        #{
          openapi => "3.0.0",
          servers => [#{url => "/"}],
          info => #{
            title => "Get Kimball API",
            version => <<"0.0.0">>
    }}),
    ok.

setup_namespace() ->
    NamespaceString = os:getenv("NAMESPACE", "getkimball"),
    NamespaceBin = binary:list_to_bin(NamespaceString),
    application:set_env(features, namespace, NamespaceBin).

setup_analytics_url() ->
    EnvVarValue = os:getenv("ANALYTICS_HOST", "undefined"),
    URL = case EnvVarValue of
        "undefined" -> undefined;
        Host -> "http://" ++ Host ++ "/v0/analytics"
    end,

    persistent_term:put({features, analytics_url}, URL),
    ok.
setup_analytics_event_mod(api_server) ->
    Mod = features_count_router,
    application:set_env(features, analytics_event_mod, Mod);
setup_analytics_event_mod(sidecar) ->
    Mod = features_count_relay,
    application:set_env(features, analytics_event_mod, Mod).

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
    {ok, VSN} = application:get_key(features, vsn),
    VSNBin = binary:list_to_bin(VSN),
    Version = << <<"features-">>/binary, VSNBin/binary >>,
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
        }}),
        ok = eraven:set_environment_context(eraven,
                                       <<"server">>,
                                       <<"environment">>,
                                       Version)
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
