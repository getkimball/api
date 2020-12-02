-module(features_bayesian_predictor).

-include_lib("kernel/include/logger.hrl").

-export([
    bayes/3,
    for_goal_counts/1,
    for_events/2
]).

bayes(BGivenA, A, B) ->
    (BGivenA * A) / B.

for_events(_Namespace, []) ->
    #{};
for_events(Namespace, Events) ->
    GlobalPredictions = for_goal_counts(Namespace),

    % Iterate over all the events and grab predictions for a each goal that has
    % a prediction for that event. Then accumulate those predictions, taking
    % just the prediction from one event (for now).
    EventMFFun = fun(MapEvent, GlobalAccIn) ->
        % Match event name to the predictions and build a goal => prediction map
        AccumFun = fun
            (Goal, #{MapEvent := Prediction}, AccIn) ->
                AccIn#{Goal => Prediction};
            (_Goal, _NoMatchPredictions, AccIn) ->
                AccIn
        end,

        % Grab predictions for each event
        EventPredictions = maps:fold(AccumFun, #{}, GlobalPredictions),

        % Merge with predictions from previous events
        MergedPredictions = maps:merge(GlobalAccIn, EventPredictions),

        {EventPredictions, MergedPredictions}
    end,

    {_PerEventPredictions, FinalPredictions} = lists:mapfoldl(EventMFFun, #{}, Events),
    FinalPredictions.

for_goal_counts(Namespace) ->
    CountMap = features_count_router:count_map(Namespace),
    GoalCounts = maps:filter(fun filter_goals_with_tagged_events/2, CountMap),
    GlobalCounterId = features_counter_id:global_counter_id(Namespace),

    ?LOG_DEBUG(#{
        what => "Prediction data",
        count_map => CountMap
    }),

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

filter_goals_with_tagged_events(_ID, #{single_tag_counts := Counts}) when map_size(Counts) == 0 ->
    false;
filter_goals_with_tagged_events(_ID, _Counts) ->
    true.
