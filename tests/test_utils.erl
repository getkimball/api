-module(test_utils).

-export([defaulted_feature_spec/2]).

defaulted_feature_spec(Name, Spec) ->
    Default = #{
      name => Name,
      boolean => false,
      rollout_start => undefined,
      rollout_end => undefined,
      user => undefined
    },
    maps:merge(Default, Spec).
