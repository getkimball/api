-module(features_grpc_relay_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(MUT, features_grpc_relay).

all() -> [{group, all}].

groups() ->
    [
        {all, [
            aa_connection_notify_event,
            ab_connection_error_will_eventually_try_to_reconnect
        ]}
    ].

init_meck(Config) ->
    test_utils:meck_load_prometheus(),
    meck:new(grpc_client),
    meck:new(timer, [unstick]),
    Config.

init_per_testcase(_, Config) ->
    NewConfig = init_meck(Config),
    NewConfig.

start(Config) ->
    Host = "host",
    Port = "port",
    {ok, Pid} = ?MUT:start_link(Host, Port),
    [
        {pid, Pid},
        {host, Host},
        {port, Port}
        | Config
    ].

notify(Config, Msg) ->
    Pid = ?config(pid, Config),
    ?MUT:notify(Pid, Msg).

end_per_testcase(_, Config) ->
    io:format("End Config ~p~n", [Config]),
    test_utils:meck_unload_prometheus(),
    ?assert(meck:validate(grpc_client)),
    ok = meck:unload(grpc_client),
    ?assert(meck:validate(timer)),
    ok = meck:unload(timer),

    ok.

aa_connection_notify_event(Config0) ->
    Conn = make_ref(),
    ES = make_ref(),

    NS = ns,
    Name = name,
    Key = key,

    Msg = #{
        namespace => NS,
        name => Name,
        key => Key
    },

    meck:expect(grpc_client, connect, ['_', '_', '_'], {ok, Conn}),
    meck:expect(
        grpc_client,
        new_stream,
        [Conn, 'KimballIntegration', 'EventStream', features_proto_pb],
        {ok, ES}
    ),
    meck:expect(grpc_client, send, [ES, '_'], ok),
    Config1 = start(Config0),
    meck:wait(grpc_client, new_stream, ['_', '_', '_', '_'], 1000),
    notify(Config1, {NS, Name, Key}),

    test_utils:assertNCalls(1, grpc_client, connect, [
        tcp,
        ?config(host, Config1),
        ?config(port, Config1)
    ]),

    meck:wait(grpc_client, send, [ES, Msg], 250),
    test_utils:assertNCalls(1, grpc_client, send, [ES, Msg]),

    Config1.

ab_connection_error_will_eventually_try_to_reconnect(Config0) ->
    meck:expect(grpc_client, connect, ['_', '_', '_'], {error, "test error"}),
    meck:expect(timer, apply_after, ['_', gen_statem, cast, ['_', connect]], {ok, make_ref()}),

    Config1 = start(Config0),
    Pid = ?config(pid, Config1),

    meck:wait(timer, apply_after, '_', 500),
    test_utils:assertNCalls(1, grpc_client, connect, [
        tcp,
        ?config(host, Config1),
        ?config(port, Config1)
    ]),
    test_utils:assertNCalls(1, timer, apply_after, ['_', gen_statem, cast, [Pid, connect]]),

    Config1.
