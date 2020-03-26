-module(hellerl_world_app).
-behaviour(application).
-include_lib("kernel/include/logger.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    ?LOG_INFO(#{what=><<"Starting Hellerl World">>}),
    App = hellerl_world,
    Routes = [
        {"/metrics/[:registry]", prometheus_cowboy2_handler, []}
    ],

    ModRoutes = cowboy_route_setup:get_routes_from_modules([
        hellerl_world_handler_ok,
        hellerl_world_handler_incoming
    ]),

    StaticRoute = [
        {"/[...]", cowboy_static, {priv_dir, App, "public"}}
    ],

    AllRoutes = Routes ++ ModRoutes ++ StaticRoute,

    Dispatch = cowboy_router:compile([{'_', AllRoutes}]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
      env => #{dispatch => Dispatch},
      metrics_callback => fun prometheus_cowboy2_instrumenter:observe/1,
      stream_handlers => [cowboy_metrics_h, cowboy_stream_h]
    }),
    hellerl_world_sup:start_link().

stop(_State) ->
  ok.
