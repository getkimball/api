-module(hellerl_world_handler_ok).

-export([init/2]).

init(Req0, Opts) ->
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"ok!">>, Req0),
    {ok, Req, Opts}.
