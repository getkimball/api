-module(features_counter_persist_manager_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(MUT, features_counter_persist_manager).

all() -> [{group, all}].

groups() ->
    [
        {all, [
            aa_test_timer_is_setup,
            ab_test_single_counter_is_persisted
        ]}
    ].

init_meck(Config) ->
    test_utils:meck_load_prometheus(),
    meck:new(features_count_router),

    meck:new(timer, [unstick]),
    meck:expect(timer, apply_interval, ['_', '_', '_', '_'], {ok, tref}),

    meck:new(features_counter),
    meck:expect(features_counter, persist, ['_'], ok),

    Config.

init_per_testcase(_, Config) ->
    NewConfig = init_meck(Config),

    {ok, Pid} = ?MUT:start_link(),
    [{pid, Pid} | NewConfig].

end_per_testcase(_, _Config) ->
    test_utils:meck_unload_prometheus(),
    ?assert(meck:validate(timer)),
    meck:unload(timer),

    ?assert(meck:validate(features_count_router)),
    meck:unload(features_count_router),

    ?assert(meck:validate(features_counter)),
    meck:unload(features_counter),
    ok.

aa_test_timer_is_setup(Config) ->
    % Pid = ?config(pid, Config),

    % User = <<"user_id">>,
    meck:wait(timer, apply_interval, '_', 1000),
    ?assertEqual(1, meck:num_calls(timer, apply_interval, [60000, ?MUT, persist, []])),

    % ?MUT:add(User, Pid),

    % Num = ?MUT:count(Pid),

    % ?assertEqual(counts(#{count => 1,
    %                       single_tag_counts => #{},
    %                       tag_counts => #{[] => 1}}), Num),
    Config.

ab_test_single_counter_is_persisted(Config) ->
    CounterPid = self(),

    meck:expect(features_count_router, counter_pids, [], [CounterPid]),

    ?MUT:persist(),

    meck:wait(features_counter, persist, '_', 1000),
    io:format("Calls ~p~n", [meck:history(features_counter)]),
    ?assertEqual(1, meck:num_calls(features_counter, persist, [CounterPid])),

    Config.
