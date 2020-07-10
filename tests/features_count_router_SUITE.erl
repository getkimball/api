-module(features_count_router_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").


-define(MUT, features_count_router).
-define(COUNTER_MOD, features_counter).

all() -> [{group, test_count}].

groups() -> [{test_count, [
                aa_test_new_counter,
                ab_test_existing_counter,
                ac_test_counter_registration_race,
                ba_test_counter_counts
              ]}
            ].

init_per_testcase(_, Config) ->
    meck:new(?COUNTER_MOD),
    meck:expect(?COUNTER_MOD, add, ['_', '_'], ok),

    meck:new(supervisor, [unstick]),
    {ok, Pid} = ?MUT:start_link(),
    [{pid, Pid}|Config].

end_per_testcase(_, _Config) ->
    ?assert(meck:validate(?COUNTER_MOD)),
    meck:unload(?COUNTER_MOD),

    ?assert(meck:validate(supervisor)),
    meck:unload(supervisor),
    ok.

aa_test_new_counter(_Config) ->
    Feature = <<"feature_name">>,
    Pid = self(),
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, Pid}),
    User = <<"user_id">>,

    ?MUT:add(Feature, User),

    Spec = #{id => {features_counter, Feature},
             start => {features_counter, start_link, [Feature]}},

    ?assertEqual(Spec, meck:capture(first, supervisor, start_child, '_', 2)),
    ?assertEqual(User, meck:capture(first, ?COUNTER_MOD, add, '_', 1)),
    ?assertEqual(Pid, meck:capture(first, ?COUNTER_MOD, add, '_', 2)).

ab_test_existing_counter(_Config) ->
    Feature = <<"feature_name">>,
    Pid = self(),
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, Pid}),

    User = <<"user_id">>,

    ?MUT:add(Feature, User),
    ?MUT:register_counter(Feature, Pid),
    ?MUT:add(Feature, User),

    Spec = #{id => {features_counter, Feature},
             start => {features_counter, start_link, [Feature]}},

    ?assertEqual(Spec, meck:capture(first, supervisor, start_child, '_', 2)),
    ?assertError(not_found, meck:capture(2, supervisor, start_child, '_', 2)),

    ?assertEqual(User, meck:capture(first, ?COUNTER_MOD, add, '_', 1)),
    ?assertEqual(Pid, meck:capture(first, ?COUNTER_MOD, add, '_', 2)),

    ?assertEqual(User, meck:capture(2, ?COUNTER_MOD, add, '_', 1)),
    ?assertEqual(Pid, meck:capture(2, ?COUNTER_MOD, add, '_', 2)).

ac_test_counter_registration_race(_Config) ->
    Feature = <<"feature_name">>,
    Pid = self(),
    SupResp = {error, {already_started, Pid}},
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], SupResp),

    User = <<"user_id">>,

    ?MUT:add(Feature, User),

    Spec = #{id => {features_counter, Feature},
             start => {features_counter, start_link, [Feature]}},

    ?assertEqual(Spec, meck:capture(first, supervisor, start_child, '_', 2)),

    ?assertEqual(User, meck:capture(first, ?COUNTER_MOD, add, '_', 1)),
    ?assertEqual(Pid, meck:capture(first, ?COUNTER_MOD, add, '_', 2)).

ba_test_counter_counts(_Config) ->
    Feature = <<"feature_name">>,
    Pid = self(),
    Count = 1,

    meck:expect(features_counter, count, [Pid], Count),

    ?MUT:register_counter(Feature, Pid),


    Counts = ?MUT:counts(),

    ?assertEqual([#{name => Feature, count => Count}], Counts).
