-module(features_bayesian_predictor).

-include_lib("kernel/include/logger.hrl").

-export([
    bayes/3,
    for_goal_counts/1,
    for_events/2
]).

bayes(BGivenA, A, B) ->
    (BGivenA * A) / B.

% Naive bayes for multiple events
for_events(_Namespace, []) ->
    #{};
for_events(Namespace, Events) ->
    CountMap = features_count_router:count_map(Namespace),
    GoalCounts = goal_counts_from_count_map(CountMap),

    GlobalFun = fun(FoldGoalID, #{count := GoalCount, single_tag_counts := GoalSTC}, GoalAccIn) ->
        GoalName = features_counter_id:name(FoldGoalID),
        EventCounterFun = fun(Event) ->
            GoalEventCount = maps:get(Event, GoalSTC, 0),
            EventID = features_counter_id:create(Namespace, Event, named),
            EventCount =
                case maps:is_key(EventID, CountMap) of
                    true ->
                        #{EventID := #{count := EventCountMatch}} = CountMap,
                        EventCountMatch;
                    false ->
                        throw({unknown_event, Event})
                end,

            EventNotGoalCount = EventCount - GoalEventCount,

            YesEventChange = GoalEventCount / GoalCount,
            NoEventChange = EventNotGoalCount / GoalCount,

            {YesEventChange, NoEventChange}
        end,

        % Don't run if goal is part of events, we'll skip them for now
        case lists:member(GoalName, Events) of
            true ->
                Note = <<"Submitted events included this goal, no prediction made">>,
                GoalAccIn#{GoalName => #{note => Note}};
            false ->
                EventCounts = lists:map(EventCounterFun, Events),

                {EventSumYes, EventSumNo} = lists:foldl(
                    fun({Py, Pn}, {AccY, AccN}) -> {Py * AccY, Pn * AccN} end,
                    {1, 1},
                    EventCounts
                ),
                Likelihood = EventSumYes / EventSumNo,
                GoalAccIn#{GoalName => #{yes => EventSumYes, no => EventSumNo, likelihood => Likelihood}}
        end
    end,
    GlobalGoalPredictions = maps:fold(GlobalFun, #{}, GoalCounts),

    GlobalGoalPredictions.

% Bayes calculation
for_goal_counts(Namespace) ->
    CountMap = features_count_router:count_map(Namespace),
    GoalCounts = goal_counts_from_count_map(CountMap),
    GlobalCounterId = features_counter_id:global_counter_id(Namespace),

    #{GlobalCounterId := #{count := GlobalCount}} = CountMap,

    CalcFun = fun(
        GoalID,
        #{
            count := GoalCount,
            single_tag_counts := STC
        },
        GoalAccIn
    ) ->
        GoalName = features_counter_id:name(GoalID),

        TagMapFun = fun(Tag, GoalTagCount) ->
            TagID = features_counter_id:create(Namespace, Tag, named),
            #{TagID := #{count := TagCount}} = CountMap,
            bayes_for_counts(GlobalCount, GoalCount, TagCount, GoalTagCount)
        end,

        GoalPredictions = maps:map(TagMapFun, STC),
        GoalAccIn#{GoalName => GoalPredictions}
    end,

    Predictions = maps:fold(CalcFun, #{}, GoalCounts),

    Predictions.

bayes_for_counts(GlobalCount, GoalCount, TagCount, GoalTagCount) ->
    BGivenA = GoalTagCount / GoalCount,
    A = GoalCount / GlobalCount,
    B = TagCount / GlobalCount,

    bayes(BGivenA, A, B).

goal_counts_from_count_map(CountMap) ->
    maps:filter(fun filter_goals_with_tagged_events/2, CountMap).

filter_goals_with_tagged_events(_ID, #{single_tag_counts := Counts}) when map_size(Counts) == 0 ->
    false;
filter_goals_with_tagged_events(_ID, _Counts) ->
    true.
