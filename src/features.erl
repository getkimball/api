-module(features).
-include_lib("kernel/include/logger.hrl").
-export([collapse_to_boolean/4,
         collapse_features_to_map/2]).

% Default case, only boolean defined
collapse_to_boolean(#{name := Name,
                      boolean := Boolean,
                      rollout_end := undefined,
                      user := []},
                    _User,
                    Now,
                    _Rand) ->
    ?LOG_DEBUG(#{what => "Collapse boolean",
                 now => Now}),
    {Name, Boolean};
% Rollout defined
collapse_to_boolean(#{name := Name,
                      rollout_start := Start,
                      rollout_end := End,
                      user := []},
                    _User,
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
    {Name, Boolean};
% User spec
collapse_to_boolean(Spec = #{name := Name,
                             user := UserSpec},
                    User,
                    Now,
                    Rand) ->
    CollapsedUserVal = collapse_user_with_user_spec(User, UserSpec),
    case CollapsedUserVal of
        true -> {Name, true};
        false -> SpecWithoutUser = maps:put(user, [], Spec),
                 collapse_to_boolean(SpecWithoutUser, User, Now, Rand)
    end.

collapse_features_to_map(Features, User) ->
    Now = erlang:system_time(seconds),
    Rand = rand:uniform(),

    Collapsed = [collapse_to_boolean(F, User, Now, Rand) || F <- Features],
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

collapse_user_with_user_spec(_User, _Specs=[]) ->
    false;
collapse_user_with_user_spec(User, _Specs=[[Prop, '=', Val]|T]) ->
    UserVal = maps:get(Prop, User, undefined),
    case UserVal == Val of
        true -> true;
        false -> collapse_user_with_user_spec(User, T)
    end.
