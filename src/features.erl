-module(features).

-export([collapse_to_boolean/2,
         collapse_features_map/1]).

collapse_to_boolean(#{boolean := Boolean,
                      rollout_end := undefined}, _Now) ->
    Boolean;
collapse_to_boolean(#{rollout_start := Start,
                      rollout_end := End}, Now) ->
    Boolean = if
        Now =< Start -> false;
        Now >= End -> true
    end,
    Boolean.

%%%%
%   Internal
%%%%

collapse_features_map(Map) ->
    Now = erlang:system_time(seconds),
    CollapseFun = fun(K, V, AccIn) ->
      NewV = collapse_to_boolean(V, Now),
      maps:put(K, NewV, AccIn)
    end,
    maps:fold(CollapseFun, #{}, Map).
