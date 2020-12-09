-module(test_utils).

-include_lib("eunit/include/eunit.hrl").

-export([
    assertNCalls/4,
    defaulted_feature_spec/2,
    meck_load_prometheus/0,
    meck_unload_prometheus/0
]).

assertNCalls(Num, Mod, Fun, Args) ->
    io:format("Mod calls ~p: ~p~n", [Mod, meck:history(Mod)]),
    ?assertEqual(Num, meck:num_calls(Mod, Fun, Args)).

defaulted_feature_spec(Name, Spec) ->
    Default = #{
        name => Name,
        boolean => false,
        rollout_start => undefined,
        rollout_end => undefined,
        user => []
    },
    maps:merge(Default, Spec).

meck_load_prometheus() ->
    ok = meck:new(prometheus_gauge),
    meck:expect(prometheus_gauge, declare, ['_'], ok),
    meck:expect(prometheus_gauge, set, ['_', '_'], ok),
    meck:expect(prometheus_gauge, set, ['_', '_', '_', '_'], ok),

    ok = meck:new(prometheus_summary),
    meck:expect(prometheus_summary, declare, ['_'], ok),
    meck:expect(prometheus_summary, observe, ['_', '_'], ok),

    ok.

meck_unload_prometheus() ->
    ?assert(meck:validate(prometheus_gauge)),
    meck:unload(prometheus_gauge),

    ?assert(meck:validate(prometheus_summary)),
    meck:unload(prometheus_summary).
