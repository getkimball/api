-module(features_handler_v0_nudges_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_handler_v0_nudges).
-define(CTH, cowboy_test_helpers).

load() ->
    ok = ?CTH:setup(),

    ok = meck:new(features_bayesian_predictor),
    ok = meck:new(features_count_router),

    ok = meck:new(features_grpc_rpc),
    ok = meck:expect(features_grpc_rpc, predict, ['_', '_'], []),

    ok.

unload(_) ->
    ?assert(meck:validate(features_bayesian_predictor)),
    ok = meck:unload(features_bayesian_predictor),

    ?assert(meck:validate(features_count_router)),
    ok = meck:unload(features_count_router),

    ?assert(meck:validate(features_grpc_rpc)),
    ok = meck:unload(features_grpc_rpc),

    ok = ?CTH:cleanup(),
    ok.

metadata_test_() ->
    {foreach, fun load/0, fun unload/1, [fun setup/0]}.

setup() ->
    [Trail] = ?MUT:trails(),
    Path = trails:path_match(Trail),
    ?assertEqual("/v0/nudges", Path).

%%%%
%   Get analytics from router
%%%%

get_test_() ->
    {foreach, fun load/0, fun unload/1, [
        fun get_user_nudges/0,
        fun get_user_nudges_when_no_user_events/0,
        fun get_user_nudges_missing_user/0,
        fun get_user_nudges_missing_goal/0
        % fun get_user_predictions_with_api_events/0,
        % fun get_namespaced_user_predictions/0,
        % fun get_user_predictions_with_no_events/0,
        % fun get_external_predictions_for_events/0,
        % fun get_external_predictions_for_goal_counts/0
    ]}.

get_user_nudges() ->
    Goal = <<"goal_1">>,
    Predictions = #{
        Goal => #{
            <<"feature_2">> => 0.5,
            <<"foo_event">> => 0.3,
            <<"feature_1">> => 0.1
        }
    },
    Events = [<<"foo_event">>],
    UserID = <<"user_id">>,

    ok = meck:expect(
        features_count_router,
        events_for_key,
        [<<"default">>, UserID],
        Events
    ),

    ok = meck:expect(features_bayesian_predictor, for_goal_counts, ['_'], Predictions),

    ExpectedData = #{
        <<"user_id">> => UserID,
        <<"goal">> => Goal,
        <<"nudges">> => [
            #{<<"event_name">> => <<"feature_2">>, <<"prediction">> => 0.5},
            #{<<"event_name">> => <<"feature_1">>, <<"prediction">> => 0.1}
        ]
    },

    Req = ?CTH:req(get, [{<<"user_id">>, UserID}, {<<"goal">>, Goal}]),
    State = #{},
    ?CTH:http_get(?MUT, State, Req, 200, ExpectedData).

get_user_nudges_when_no_user_events() ->
    Goal = <<"goal_1">>,
    Predictions = #{
        Goal => #{
            <<"feature_2">> => 0.5,
            <<"foo_event">> => 0.3,
            <<"feature_1">> => 0.1
        }
    },
    UserID = <<"user_id">>,

    ok = meck:expect(
        features_count_router,
        events_for_key,
        [<<"default">>, UserID],
        []
    ),

    ok = meck:expect(features_bayesian_predictor, for_goal_counts, ['_'], Predictions),

    ExpectedData = #{
        <<"user_id">> => UserID,
        <<"goal">> => Goal,
        <<"nudges">> => [
            #{<<"event_name">> => <<"feature_2">>, <<"prediction">> => 0.5},
            #{<<"event_name">> => <<"foo_event">>, <<"prediction">> => 0.3},
            #{<<"event_name">> => <<"feature_1">>, <<"prediction">> => 0.1}
        ]
    },

    Req = ?CTH:req(get, [{<<"user_id">>, UserID}, {<<"goal">>, Goal}]),
    State = #{},
    ?CTH:http_get(?MUT, State, Req, 200, ExpectedData).

get_user_nudges_missing_user() ->
    Goal = <<"goal_1">>,

    Expected = #{
        <<"error">> => #{
            <<"key">> => <<"user_id">>,
            <<"what">> => <<"Missing required element">>
        }
    },

    Req = ?CTH:req(get, [{<<"goal">>, Goal}]),
    State = #{},
    ?CTH:http_get(?MUT, State, Req, 400, Expected).

get_user_nudges_missing_goal() ->
    UserID = <<"user_id">>,

    Expected = #{
        <<"error">> => #{
            <<"key">> => <<"goal">>,
            <<"what">> => <<"Missing required element">>
        }
    },

    Req = ?CTH:req(get, [{<<"user_id">>, UserID}]),
    State = #{},
    ?CTH:http_get(?MUT, State, Req, 400, Expected).
