-module(hellerl_world_handler_ok_test).
-include_lib("eunit/include/eunit.hrl").

ok_test() ->
    Req = cowboy_test_helpers:req(),
    Opts = [],

    CowResp = hellerl_world_handler_ok:init(Req, Opts),
    {response, Code, _Headers, Body} = cowboy_test_helpers:read_reply(CowResp),

    ?assertEqual(200, Code),
    ?assertEqual(<<"ok!">>, Body),
    ok.
