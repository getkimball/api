-module(test_utils).
-include_lib("eunit/include/eunit.hrl").

-export([defaulted_feature_spec/2,
         meck_load_prometheus/0,
         meck_unload_prometheus/0]).

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
    ok.

meck_unload_prometheus() ->
    ?assert(meck:validate(prometheus_gauge)),
    meck:unload(prometheus_gauge).
