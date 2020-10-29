-module(features).

-include_lib("kernel/include/logger.hrl").

-export([
    collapse_to_boolean/4,
    collapse_features_to_map/2,
    comparator_bin_to_atom/1
]).

% Default case, only boolean defined
collapse_to_boolean(
    #{name := Name, boolean := true},
    _User,
    _Now,
    _Rand
) ->
    {Name, true};
% User spec
collapse_to_boolean(
    Spec = #{
        name := Name,
        user := UserSpec
    },
    User,
    Now,
    Rand
) ->
    CollapsedUserVal = collapse_user_with_user_spec(User, UserSpec),
    case CollapsedUserVal of
        true ->
            {Name, true};
        false ->
            SpecWithoutUser = maps:remove(user, Spec),
            collapse_to_boolean(SpecWithoutUser, User, Now, Rand)
    end;
% Rollout defined
collapse_to_boolean(
    Spec = #{rollout_end := undefined},
    User,
    Now,
    Rand
) ->
    SpecWithOutEnd = maps:remove(rollout_end, Spec),
    collapse_to_boolean(SpecWithOutEnd, User, Now, Rand);
collapse_to_boolean(
    #{
        name := Name,
        rollout_start := Start,
        rollout_end := End
    },
    _User,
    Now,
    Rand
) ->
    Boolean =
        if
            % Before start of rollout
            Now =< Start ->
                false;
            % After end of rollout
            Now >= End ->
                true;
            % During rollout
            true ->
                collapse_rollout_progress(
                    Start,
                    End,
                    Now,
                    Rand
                )
        end,
    ?LOG_DEBUG(#{
        what => "Collapse boolean rollout",
        start => Start,
        quick_false => Now =< Start,
        quick_true => Now >= End,
        'end' => End,
        boolean => Boolean,
        now => Now
    }),

    {Name, Boolean};
collapse_to_boolean(#{name := Name}, _User, _Now, _Rand) ->
    {Name, false}.

collapse_features_to_map(Features, User) ->
    Now = erlang:system_time(seconds),
    Rand = rand:uniform(),

    Collapsed = [collapse_to_boolean(F, User, Now, Rand) || F <- Features],
    Map = maps:from_list(Collapsed),
    Map.

comparator_bin_to_atom(<<"=">>) -> '=';
comparator_bin_to_atom(<<"in">>) -> 'in'.

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
    ?LOG_INFO(#{
        what => "Collapse rollout",
        start => Start,
        'end' => End,
        duration => Duration,
        progress_time => ProgressTime,
        rand => Rand,
        chance => Chance
    }),
    Boolean.

collapse_user_with_user_spec(_User, _Specs = []) ->
    false;
collapse_user_with_user_spec(User, _Specs = [[Prop, '=', Val] | T]) ->
    UserVal = maps:get(Prop, User, undefined),
    ?LOG_DEBUG(#{
        what => "Collapse boolean user",
        user => User,
        user_val => UserVal,
        property => Prop,
        spec_value => Val,
        result => UserVal == Val
    }),
    case UserVal == Val of
        true -> true;
        false -> collapse_user_with_user_spec(User, T)
    end;
collapse_user_with_user_spec(User, _Specs = [[Prop, 'in', Val] | T]) ->
    UserVal = maps:get(Prop, User, undefined),
    ?LOG_DEBUG(#{
        what => "Collapse boolean user",
        user => User,
        user_val => UserVal,
        property => Prop,
        spec_value => Val,
        result => UserVal == Val
    }),
    case lists:member(UserVal, Val) of
        true -> true;
        false -> collapse_user_with_user_spec(User, T)
    end.
