-module(features_handler_v0_predictions_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_handler_v0_predictions).
-define(CTH, cowboy_test_helpers).

load() ->
    ok = ?CTH:setup(),

    ok = meck:new(features_bayesian_predictor),

    ok.

unload(_) ->
    ?assert(meck:validate(features_bayesian_predictor)),
    ok = meck:unload(features_bayesian_predictor),

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
        fun get_namespaced_prediction/0
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
