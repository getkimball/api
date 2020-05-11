-module(features_handler_v0_features).

-export([trails/0]).
-export([init/2]).


trails() ->
    [
        {"/v0/features", ?MODULE, []}
    ].


init(Req, Opts) ->
    Resp = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"ok!">>, Req),
    {ok, Resp, Opts}.
