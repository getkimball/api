-module(features_handler_unauthorized).

-export([
    init/2
]).

-include_lib("kernel/include/logger.hrl").

init(Req0, Opts) ->
    Resp = cowboy_req:reply(
        401,
        #{
            <<"content-type">> => <<"application/json">>
        },
        jsx:encode(#{error => <<"Missing or invalid authorization header">>}),
        Req0
    ),

    {ok, Resp, Opts}.
