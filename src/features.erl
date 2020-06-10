-module(features).
-include_lib("kernel/include/logger.hrl").
-export([collapse_to_boolean/3,
         collapse_features_to_map/1]).

collapse_to_boolean(#{name := Name,
                      boolean := Boolean,
                      rollout_end := undefined},
                    Now,
                    _Rand) ->
    ?LOG_DEBUG(#{what => "Collapse boolean",
                 now => Now}),
    {Name, Boolean};
collapse_to_boolean(#{name := Name,
                      rollout_start := Start,
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
    {Name, Boolean}.

collapse_features_to_map(Features) ->
    Now = erlang:system_time(seconds),
    Rand = rand:uniform(),

    Collapsed = [collapse_to_boolean(F, Now, Rand) || F <- Features],
    Map = maps:from_list(Collapsed),
    Map.


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
