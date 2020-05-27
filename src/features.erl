-module(features).
-include_lib("kernel/include/logger.hrl").
-export([collapse_to_boolean/3,
         collapse_features_map/1]).

collapse_to_boolean(#{boolean := Boolean,
                      rollout_end := undefined},
                    Now,
                    _Rand) ->
    ?LOG_DEBUG(#{what => "Collapse boolean",
                 now => Now}),
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
    ?LOG_DEBUG(#{what => "Collapse boolean rollout",
                 start => Start,
                 quick_false => Now =< Start,
                 quick_true => Now >= End,
                 'end' => End,
                 boolean => Boolean,
                 now => Now}),
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
    ?LOG_INFO(#{what => "Collapse rollout",
                start => Start,
                'end' => End,
                duration => Duration,
                progress_time => ProgressTime,
                rand => Rand,
                chance => Chance}),
    Boolean.
