-module(hellerl_world_handler_incoming).

-export([init/2]).

init(Req0 = #{method := <<"POST">>,
              headers := #{<<"content-type">> := <<"application/json">>}},
            Opts) ->
    {ok, DataBin, Req1} = cowboy_req:read_body(Req0),
    _Data = jsx:decode(DataBin, [return_maps]),
    Rep = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"POST">> , Req1),

    {ok, Rep, Opts};
init(Req0, Opts) ->
    Msg = <<"Requests should be posted with content-type: application/json">>,
    Rep = cowboy_req:reply(400, #{
        <<"content-type">> => <<"text/plain">>
    }, Msg, Req0),

    {ok, Rep, Opts}.
