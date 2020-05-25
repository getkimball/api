-module(test_utils).

-export([defaulted_feature_spec/1]).

defaulted_feature_spec(Spec) ->
    Default = #{
      boolean => false,
      rollout_start => undefined,
      rollout_end => undefined
    },
    maps:merge(Default, Spec).
