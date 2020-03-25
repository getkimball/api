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

    {ok, Rep, Opts}.
