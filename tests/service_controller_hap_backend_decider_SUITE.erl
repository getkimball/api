-module(service_controller_hap_backend_decider_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").


-define(MUT, service_controller_hap_backend_decider).
-define(TRANSPORT, decider_fake_transport).


all() -> [{group, test_swaggerl}].

groups() -> [{test_swaggerl, [
                aa_happy_case
              ]}
            ].


init_per_testcase(_, Config) ->
    ok = meck:new(ranch, []),
    ok = meck:new(service_controller_service_watcher, []),
    ok = meck:new(?TRANSPORT, [non_strict]),

    InitRef = make_ref(),
    Socket = make_ref(),
    ok = meck:expect(ranch, handshake, fun(Ref) ->
        ?assertEqual(InitRef, Ref),
        {ok, Socket} end),

    StartFun = fun() ->
      ?MUT:start_link(InitRef, Socket, ?MODULE, [])
    end,

    [{init_ref, InitRef}, {socket, Socket}, {start_fun, StartFun} | Config].

end_per_testcase(_, Config) ->
    meck:unload(?TRANSPORT),
    meck:unload(service_controller_service_watcher),
    meck:unload(ranch),
    Config.

aa_happy_case(Config) ->
    Backend = make_ref(),
    Backends = [Backend],
    Frontend = make_ref(),

    FakeRecv = fun(Socket, Length, Timeout) ->
      ?assertEqual(?config(socket, Config), Socket),
      ?assertEqual(0, Length),
      ?assertEqual(100, Timeout),
      {ok, Frontend}
    end,

    meck:expect(?TRANSPORT, recv, FakeRecv),
    meck:expect(service_controller_service_watcher, backends,
                fun(Name) ->
                  ?assertEqual(Frontend, Name),
                  Backends
                end),

    StartFun = ?config(start_fun, Config),
    {ok, _Pid} = StartFun(),

    ok.

async_http_send(Pid, Body) ->
    Pid ! Body.

pet_operations() ->
    ["addPet",
     "deletePet",
     "find pet by id",
     "findPets"].

get_msg() ->
    receive
        Msg -> Msg
    after 1000 ->
        error
    end.
