-module(features_app).

-behaviour(application).

-include_lib("kernel/include/logger.hrl").

-export([start/2]).
-export([stop/1]).

-export([decide_store_lib/0]).

-define(DEFAULT_GCS_BUCKET, "default_bucket").
-define(DEFAULT_S3_BUCKET, "default_bucket").

start(_Type, _Args) ->
    {ok, VSN} = application:get_key(features, vsn),
    ?LOG_INFO(#{
        what => <<"Starting">>,
        version => VSN
    }),
    App = features,
    Mode = get_features_mode(),

    ok = set_config(Mode),
    MetricsOpts = metrics_opts(),
    ok = features_counter_config:validate_config(),
    StoreLib = decide_store_lib(),
    io:format("Application: ~p~n", [application:get_application()]),

    Routes = [
        {"/metrics/[:registry]", prometheus_cowboy2_handler, []}
    ],

    % The priv_dir used by cowboy relies on the Erlang's view of the filesystem
    % path of this project in order to find priv. This can be problematic if
    % the directory name doesn't match the application name. Allow overriding
    % this to manually find the path. During releases the application/path all
    % lines up... but humans may do it differently than the release!
    IsLocalDev = application:get_env(features, local_dev, false),
    StaticRoute = [
        {"/", cowboy_static, cowboy_priv_path_for_file(IsLocalDev, App, "public/index.html")},
        {"/[...]", cowboy_static, cowboy_priv_path_for_dir(IsLocalDev, App, "public")}
    ],

    Trails = setup_trails(),

    AllRoutes = Routes ++ Trails ++ StaticRoute,

    Dispatch = trails:single_host_compile(AllRoutes),

    Middlewares =
        case application:get_env(features, api_auth, enable) of
            enable -> [cowboy_router, cb_auth_simple_header, cowboy_handler];
            disable -> [cowboy_router, cowboy_handler]
        end,

    CBAuthOpts = #{
        auth_tokens => application:get_env(features, api_auth_tokens, []),
        exclude_handlers => [features_handler_ok],
        unauth_handler => features_handler_unauthorized
    },

    HTTPOpts = #{
        env => #{
            dispatch => Dispatch,
            cb_auth => CBAuthOpts
        },
        metrics_callback => fun prometheus_cowboy2_instrumenter:observe/1,
        middlewares => Middlewares,
        stream_handlers => [cowboy_metrics_h, cowboy_stream_h]
    },

    % Heroku uses just "PORT" as an envvar, but we'll leave a more specific version as well
    Port = os:getenv("PORT", "8080"),
    APIPort = list_to_integer(os:getenv("API_PORT", Port)),

    {ok, _} = cowboy:start_clear(http, [{port, APIPort}], HTTPOpts),
    Start = features_sup:start_link(Mode, StoreLib, MetricsOpts),
    add_grpc_handlers(),
    Start.

stop(_State) ->
    ok.

metrics_opts() ->
    MemLimitStr = os:getenv("KUBERNETES_MEMORY_LIMIT", "0"),
    {MemLimit, []} = string:to_integer(MemLimitStr),
    Opts = #{memory_limit => MemLimit},
    Opts.

trails_handlers() ->
    Handlers = [
        features_handler_ok,
        features_handler_v0_analytics,
        features_handler_v0_features,
        features_handler_v0_feature_specs,
        features_handler_v0_goals,
        features_handler_v0_namespaces,
        features_handler_v0_nudges,
        features_handler_v0_predictions,
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
    Namespace = setup_namespace(),
    StoragePathPrefix = os:getenv("STORAGE_PATH_PREFIX", Namespace),
    setup_additional_namespace_config(),
    setup_file_store_path(),
    setup_analytics_url(),
    setup_analytics_event_mod(Mode),
    setup_s3(StoragePathPrefix),
    setup_gcs(StoragePathPrefix),

    ok = application:set_env(trails, api_root, "/"),
    ok = application:set_env(features, mode, Mode),
    ok = application:set_env(
        cowboy_swagger,
        global_spec,
        #{
            openapi => "3.0.0",
            servers => [#{url => "/"}],
            info => #{
                title => "Get Kimball API",
                version => <<"0.0.0">>
            }
        }
    ),
    ok.

setup_namespace() ->
    NamespaceString = os:getenv("NAMESPACE", "getkimball"),
    NamespaceBin = binary:list_to_bin(NamespaceString),
    application:set_env(features, namespace, NamespaceBin),
    NamespaceString.

setup_analytics_url() ->
    EnvVarValue = os:getenv("ANALYTICS_HOST", "undefined"),
    URL =
        case EnvVarValue of
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

setup_gcs(Namespace) ->
    EnvVarValue = os:getenv("GCS_BUCKET", ?DEFAULT_GCS_BUCKET),
    GAC_EnvVarValue = os:getenv("GOOGLE_APPLICATION_CREDENTIALS", ""),
    application:set_env(features, gcs_bucket, EnvVarValue),
    application:set_env(features, gcs_base_path, Namespace),
    application:set_env(features, gcs_credentials_path, GAC_EnvVarValue).

setup_s3(Namespace) ->
    EnvVarValue = os:getenv("S3_BUCKET", ?DEFAULT_S3_BUCKET),
    S3Host = os:getenv("S3_HOST", ""),
    application:set_env(features, s3_bucket, EnvVarValue),
    application:set_env(features, s3_base_path, Namespace),
    application:set_env(features, s3_host, S3Host).

setup_additional_namespace_config() ->
    NamespacesString = os:getenv("ADDITIONAL_NAMESPACES", ""),
    NamespacesBin = binary:list_to_bin(NamespacesString),
    Namespaces = binary:split(NamespacesBin, <<",">>),
    Config = additional_namespaces_to_list(Namespaces),
    application:set_env(features, namespaces, Config),

    ?LOG_INFO(#{
        what => "Sync to additional namespaces",
        namespaces => Namespaces
    }),
    ok.

decide_store_lib() ->
    S3Set =
        case application:get_env(features, s3_bucket) of
            {ok, ?DEFAULT_S3_BUCKET} -> false;
            {ok, ""} -> false;
            {ok, _S3BucketWasSet} -> true
        end,

    GCSSet =
        case application:get_env(features, gcs_bucket) of
            {ok, ?DEFAULT_GCS_BUCKET} -> false;
            {ok, ""} -> false;
            {ok, _GCSBucketWasSet} -> true
        end,

    Lib =
        case {S3Set, GCSSet} of
            {true, true} -> throw({not_supported, multiple_storage_set});
            {true, _} -> features_store_lib_s3;
            {_, true} -> features_store_lib_gcs;
            {false, false} -> undefined
        end,

    Lib.

% Removes empty response from split
additional_namespaces_to_list([<<>>]) ->
    [];
additional_namespaces_to_list(List) ->
    List.

setup_sentry() ->
    DeploySite = application:get_env(features, site, "Unknown site"),
    DeployCluster = application:get_env(features, cluster, "Unknown cluster"),

    % Sentry has environment and server as first-class tags, in our case we'll
    % use the "site" as the environment, then the "cluster" as the server.
    Server = DeployCluster,
    Environment = DeploySite,

    {ok, VSN} = application:get_key(features, vsn),
    VSNBin = binary:list_to_bin(VSN),
    Version = <<<<"features-">>/binary, VSNBin/binary>>,
    DSN = os:getenv("SENTRY_DSN"),
    case DSN of
        false ->
            ?LOG_INFO(#{what => "Sentry not setup. Set 'SENTRY_DSN'"});
        ActualDSN ->
            ?LOG_INFO(#{what => "Sentry configured"}),
            ok = logger:add_handler(
                eraven,
                er_logger_handler,
                #{
                    level => warning,
                    config => #{
                        dsn => ActualDSN
                    }
                }
            ),
            ok = eraven:set_environment_context(
                eraven,
                Server,
                Environment,
                Version
            )
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
    case Mode of
        "sidecar" -> sidecar;
        "api" -> api_server;
        _ -> api_server
    end.

add_grpc_handlers() ->
    Targets = application:get_env(features, external_grpc_event_targets, []),
    lists:foreach(fun add_grpc_handler/1, Targets).

add_grpc_handler({Host, Port}) ->
    ?LOG_INFO(#{
        what => external_grpc_target,
        host => Host,
        port => Port
    }),
    features_grpc_sup:start_relay(Host, Port).

cowboy_priv_path_for_file(_IsLocalDev = true, _App, Path) ->
    {file, "priv/" ++ Path};
cowboy_priv_path_for_file(_IsLocalDev = false, App, Path) ->
    {priv_file, App, Path}.

cowboy_priv_path_for_dir(_IsLocalDev = true, _App, Path) ->
    {dir, "priv/" ++ Path};
cowboy_priv_path_for_dir(_IsLocalDev = false, App, Path) ->
    {priv_dir, App, Path}.
