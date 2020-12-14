-module(metrics_server_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(MUT, metrics_server).

all() -> [{group, all}].

groups() ->
    [
        {all, [
            aa_test_timer_is_setup,
            ab_test_mem_remaning,
            ac_test_prediction_metrics
        ]}
    ].

init_meck(Config) ->
    test_utils:meck_load_prometheus(),

    meck:new(timer, [unstick]),
    meck:expect(timer, apply_interval, ['_', '_', '_', '_'], {ok, tref}),

    ok = meck:new(features_bayesian_predictor),
    ok = meck:expect(features_bayesian_predictor, for_goal_counts, ['_'], #{}),

    Config.

init_per_testcase(_, Config) ->
    NewConfig = init_meck(Config),

    Memory = 100000000,

    Opts = #{memory_limit => Memory},

    {ok, Pid} = ?MUT:start_link(Opts),
    [
        {pid, Pid},
        {memory, Memory}
        | NewConfig
    ].

end_per_testcase(_, Config) ->
    Pid = ?config(pid, Config),
    ok = gen_server:stop(Pid),
    test_utils:meck_unload_prometheus(),
    ?assert(meck:validate(timer)),

    ?assert(meck:validate(features_bayesian_predictor)),
    ok = meck:unload(features_bayesian_predictor),

    meck:unload(timer),
    ok.

aa_test_timer_is_setup(Config) ->
    meck:wait(timer, apply_interval, '_', 1000),
    ?assertEqual(1, meck:num_calls(timer, apply_interval, [15000, ?MUT, tick, []])),

    Config.

ab_test_mem_remaning(Config) ->
    ?MUT:tick(),

    meck:wait(1, prometheus_gauge, set, [memory_remaining_bytes, '_'], 1000),
    io:format("Calls ~p~n", [meck:history(prometheus_gauge)]),

    % Tick in this test will call it, as well as the init for the server
    ?assertEqual(1, meck:num_calls(prometheus_gauge, set, [memory_remaining_bytes, '_'])),

    Config.

ac_test_prediction_metrics(Config) ->
    Predictions = #{
        <<"goal_1">> => #{<<"feature_1">> => 0.5}
    },
    ok = meck:expect(features_bayesian_predictor, for_goal_counts, ['_'], Predictions),

    ?MUT:tick(),

    meck:wait(1, prometheus_gauge, set, [predictions, kimball_bayes_prediction, '_', '_'], 1000),
    io:format("Calls ~p~n", [meck:history(prometheus_gauge)]),

    % Tick in this test will call it, as well as the init for the server
    Labels = [<<"goal_1">>, <<"feature_1">>],
    ?assertEqual(
        1,
        meck:num_calls(prometheus_gauge, set, [predictions, kimball_bayes_prediction, Labels, 0.5])
    ),

    Config.
