-module(hellerl_world_app).
-behaviour(application).
-include_lib("kernel/include/logger.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    ?LOG_INFO(#{what=><<"Starting Hellerl World">>}),
    App = hellerl_world,
    Dispatch = cowboy_router:compile([
      {'_', [
        {"/ready", hellerl_world_handler_ok, []},
        {"/alive", hellerl_world_handler_ok, []},
        {"/[...]", cowboy_static, {priv_dir, App, "public"}}
      ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
      env => #{dispatch => Dispatch}
    }),
    hellerl_world_sup:start_link().

stop(_State) ->
  ok.
