-module(service_controller_handler_ok_test).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, service_controller_handler_ok).

setup_test() ->
    Setup = ?MUT:setup(),
    Routes = maps:get(routes, Setup),

    ?assert(lists:member({"/ready", ?MUT, []}, Routes)),
    ?assert(lists:member({"/alive", ?MUT, []}, Routes)),

    ok.

ok_test() ->
    Req = cowboy_test_helpers:req(),
    Opts = [],

    CowResp = service_controller_handler_ok:init(Req, Opts),
    {response, Code, _Headers, Body} = cowboy_test_helpers:read_reply(CowResp),

    ?assertEqual(200, Code),
    ?assertEqual(<<"ok!">>, Body),
    ok.
