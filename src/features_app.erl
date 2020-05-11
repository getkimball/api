-module(features_app).
-behaviour(application).
-include_lib("kernel/include/logger.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    ?LOG_INFO(#{what=><<"Starting">>}),
    setup_sentry(),
    App = features,

    Routes = [
        {"/metrics/[:registry]", prometheus_cowboy2_handler, []}
    ],
    StaticRoute = [
        {"/[...]", cowboy_static, {priv_dir, App, "public"}}
    ],

    Handlers = [
        features_handler_ok
    ],
    Trails = trails:trails(Handlers),

    AllRoutes = Routes ++ Trails ++  StaticRoute,

    Dispatch = trails:single_host_compile(AllRoutes),

    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
      env => #{dispatch => Dispatch},
      metrics_callback => fun prometheus_cowboy2_instrumenter:observe/1,
      stream_handlers => [cowboy_metrics_h, cowboy_stream_h]
    }),
    features_sup:start_link().

stop(_State) ->
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
