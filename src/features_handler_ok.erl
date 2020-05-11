-module(features_handler_ok).

-export([init/2,
         trails/0]).

-include_lib("kernel/include/logger.hrl").

trails() ->
    [
        {"/ready", ?MODULE, []},
        {"/alive", ?MODULE, []}
    ].

init(Req0, Opts) ->
    Resp = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"ok!">>, Req0),

    {ok, Resp, Opts}.
