-module(features_bayesian_predictor).

-export([
    bayes/3,
    for_events/1,
    for_goal_counts/0
]).

bayes(BGivenA, A, B) ->
    (BGivenA * A) / B.

for_events(_Events) ->
    #{}.

for_goal_counts() ->
    CountMap = features_count_router:count_map(),
    GoalCounts = maps:filter(fun filter_goals_with_tagged_events/2, CountMap),
    GlobalCounterId = features_counter_id:global_counter_id(),
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
            TagID = features_counter_id:create(Tag),
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
