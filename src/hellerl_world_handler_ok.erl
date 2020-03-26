-module(hellerl_world_handler_ok).
-behaviour(cowboy_route_setup).

-export([init/2,
         setup/0]).

setup() ->
    #{routes => [{"/ready", ?MODULE, []},
                 {"/alive", ?MODULE, []}]}.

init(Req0, Opts) ->
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"ok!">>, Req0),
    {ok, Req, Opts}.
