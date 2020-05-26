-module(features).

-export([collapse_to_boolean/3,
         collapse_features_map/1]).

collapse_to_boolean(#{boolean := Boolean,
                      rollout_end := undefined},
                    _Now,
                    _Rand) ->
    Boolean;
collapse_to_boolean(#{rollout_start := Start,
                      rollout_end := End},
                    Now,
                    Rand) ->
    Boolean = if
        % Before start of rollout
        Now =< Start -> false;
        % After end of rollout
        Now >= End -> true;
        % During rollout
        true -> collapse_rollout_progress(
                  Start, End, Now, Rand)
    end,
    Boolean.

collapse_features_map(Map) ->
    Now = erlang:system_time(seconds),
    Rand = rand:uniform(),
    CollapseFun = fun(K, V, AccIn) ->
      NewV = collapse_to_boolean(V, Now, Rand),
      maps:put(K, NewV, AccIn)
    end,
    maps:fold(CollapseFun, #{}, Map).

%%%%
%   Internal
%%%%


% Compares the Ran float (0..1) to the progress of time through the rollout
% interval true if Rand is less than the progress, false otherwise
collapse_rollout_progress(Start, End, Now, Rand) ->
    Duration = End - Start,
    ProgressTime = Now - Start,
    Chance = ProgressTime / Duration,

    Boolean = Rand =< Chance,
    Boolean.
