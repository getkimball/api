-module(features_handler_v0_predictions_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_handler_v0_predictions).
-define(CTH, cowboy_test_helpers).

load() ->
    ok = ?CTH:setup(),

    ok = meck:new(features_bayesian_predictor),
    ok = meck:new(features_count_router),

    ok.

unload(_) ->
    ?assert(meck:validate(features_bayesian_predictor)),
    ok = meck:unload(features_bayesian_predictor),

    ?assert(meck:validate(features_count_router)),
    ok = meck:unload(features_count_router),

    ok = ?CTH:cleanup(),
    ok.

metadata_test_() ->
    {foreach, fun load/0, fun unload/1, [fun setup/0]}.

setup() ->
    [Trail] = ?MUT:trails(),
    Path = trails:path_match(Trail),
    ?assertEqual("/v0/predictions", Path).

%%%%
%   Get analytics from router
%%%%

get_test_() ->
    {foreach, fun load/0, fun unload/1, [
        fun get_empty_predictions/0,
        fun get_single_prediction/0,
        fun get_namespaced_prediction/0,
        fun get_event_predictions/0,
        fun get_event_predictions_multiple_events/0,
        fun get_namespaced_event_predictions/0,
        fun get_event_predictions_with_unknown_event/0,
        fun get_user_predictions/0,
        fun get_namespaced_user_predictions/0,
        fun get_user_predictions_with_no_events/0
    ]}.

get_empty_predictions() ->
    Predictions = #{},

    ok = meck:expect(features_bayesian_predictor, for_goal_counts, [<<"default">>], Predictions),

    ExpectedData = #{<<"goals">> => #{}},

    Req = ?CTH:req(),
    State = #{},
    ?CTH:http_get(?MUT, State, Req, 200, ExpectedData).

get_single_prediction() ->
    Predictions = #{
        <<"goal_1">> => #{<<"feature_1">> => 0.5}
    },

    ok = meck:expect(features_bayesian_predictor, for_goal_counts, ['_'], Predictions),

    ExpectedData = #{
        <<"goals">> => #{
            <<"goal_1">> => #{
                <<"events">> => #{
                    <<"feature_1">> => #{
                        <<"bayes">> => 0.5
                    }
                }
            }
        }
    },

    Req = ?CTH:req(),
    State = #{},
    ?CTH:http_get(?MUT, State, Req, 200, ExpectedData).

get_namespaced_prediction() ->
    Namespace = <<"test namespace">>,
    Predictions = #{
        <<"goal_1">> => #{<<"feature_1">> => 0.5}
    },

    ok = meck:expect(features_bayesian_predictor, for_goal_counts, [Namespace], Predictions),

    ExpectedData = #{
        <<"goals">> => #{
            <<"goal_1">> => #{
                <<"events">> => #{
                    <<"feature_1">> => #{
                        <<"bayes">> => 0.5
                    }
                }
            }
        }
    },

    Req = ?CTH:req(get, [{<<"namespace">>, Namespace}]),
    State = #{},
    ?CTH:http_get(?MUT, State, Req, 200, ExpectedData),
    test_utils:assertNCalls(1, features_bayesian_predictor, for_goal_counts, [Namespace]).

get_event_predictions() ->
    Predictions = #{
        <<"goal_1">> => 0.5
    },

    ok = meck:expect(
        features_bayesian_predictor,
        for_events,
        [<<"default">>, [<<"foo">>]],
        Predictions
    ),

    ExpectedData = #{
        <<"goals">> => #{
            <<"goal_1">> => 0.5
        }
    },

    Req = ?CTH:req(get, [{<<"event">>, <<"foo">>}]),
    State = #{},
    ?CTH:http_get(?MUT, State, Req, 200, ExpectedData).

get_event_predictions_multiple_events() ->
    Predictions = #{
        <<"goal_1">> => 0.5
    },

    ok = meck:expect(
        features_bayesian_predictor,
        for_events,
        [<<"default">>, [<<"bar">>, <<"foo">>]],
        Predictions
    ),

    ExpectedData = #{
        <<"goals">> => #{
            <<"goal_1">> => 0.5
        }
    },

    Req = ?CTH:req(get, [{<<"event">>, <<"foo">>}, {<<"event">>, <<"bar">>}]),
    State = #{},
    ?CTH:http_get(?MUT, State, Req, 200, ExpectedData).

get_namespaced_event_predictions() ->
    Namespace = <<"test namespace">>,
    Predictions = #{
        <<"goal_1">> => 0.5
    },

    ok = meck:expect(
        features_bayesian_predictor,
        for_events,
        [Namespace, [<<"bar">>, <<"foo">>]],
        Predictions
    ),

    ExpectedData = #{
        <<"goals">> => #{
            <<"goal_1">> => 0.5
        }
    },

    Req = ?CTH:req(get, [
        {<<"namespace">>, Namespace},
        {<<"event">>, <<"foo">>},
        {<<"event">>, <<"bar">>}
    ]),
    State = #{},
    ?CTH:http_get(?MUT, State, Req, 200, ExpectedData).

get_event_predictions_with_unknown_event() ->
    ok = meck:expect(
        features_bayesian_predictor,
        for_events,
        [<<"default">>, [<<"bar">>, <<"foo">>]],
        meck:raise(throw, {unknown_event, <<"foo">>})
    ),

    ExpectedData = #{
        <<"error">> => #{
            <<"what">> => <<"Event is not known in this namespace">>,
            <<"event">> => <<"foo">>
        }
    },

    Req = ?CTH:req(get, [{<<"event">>, <<"foo">>}, {<<"event">>, <<"bar">>}]),
    State = #{},
    ?CTH:http_get(?MUT, State, Req, 400, ExpectedData).

get_user_predictions() ->
    Predictions = #{
        <<"goal_1">> => 0.5
    },
    Events = [<<"foo_event">>],
    UserID = <<"user_id">>,

    ok = meck:expect(
        features_count_router,
        events_for_key,
        [<<"default">>, UserID],
        Events
    ),

    ok = meck:expect(
        features_bayesian_predictor,
        for_events,
        [<<"default">>, Events],
        Predictions
    ),

    ExpectedData = #{
        <<"goals">> => #{
            <<"goal_1">> => 0.5
        }
    },

    Req = ?CTH:req(get, [{<<"user_id">>, UserID}]),
    State = #{},
    ?CTH:http_get(?MUT, State, Req, 200, ExpectedData).

get_namespaced_user_predictions() ->
    Namespace = <<"test namespace">>,
    Predictions = #{
        <<"goal_1">> => 0.5
    },
    Events = [<<"foo_event">>],
    UserID = <<"user_id">>,

    ok = meck:expect(
        features_count_router,
        events_for_key,
        [Namespace, UserID],
        Events
    ),

    ok = meck:expect(
        features_bayesian_predictor,
        for_events,
        [Namespace, Events],
        Predictions
    ),

    ExpectedData = #{
        <<"goals">> => #{
            <<"goal_1">> => 0.5
        }
    },

    Req = ?CTH:req(get, [
        {<<"namespace">>, Namespace},
        {<<"user_id">>, UserID}
    ]),
    State = #{},
    ?CTH:http_get(?MUT, State, Req, 200, ExpectedData).

get_user_predictions_with_no_events() ->
    Namespace = <<"default">>,
    Events = [],
    UserID = <<"user_id">>,

    ok = meck:expect(
        features_count_router,
        events_for_key,
        [Namespace, UserID],
        Events
    ),

    ExpectedData = #{
        <<"error">> => #{
            <<"what">> => <<"No events found for user">>,
            <<"user">> => UserID
        }
    },

    Req = ?CTH:req(get, [{<<"user_id">>, UserID}]),
    State = #{},
    ?CTH:http_get(?MUT, State, Req, 404, ExpectedData).
