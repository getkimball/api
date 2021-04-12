-module(features_handler_v0_nudges).

-include_lib("kernel/include/logger.hrl").

-export([trails/0]).
-export([init/2]).

-export([
    handle_req/4,
    post_req/2
]).

-callback add(binary(), binary()) -> ok.

trails() ->
    Metadata = #{
        <<"get">> => #{
            operationId => getAnalytics,
            tags => ["Analytics"],
            description => "Gets features analytics",
            parameters => [
                #{
                    name => goal,
                    description =>
                        <<"Goal to nudge user towards">>,
                    in => query,
                    schema => #{
                        type => string
                    },
                    required => true
                },
                #{
                    name => namespace,
                    description =>
                        <<"Namespace of events">>,
                    in => query,
                    schema => #{
                        type => string,
                        default => <<"default">>
                    },
                    required => false
                },
                #{
                    name => user_id,
                    description =>
                        <<"User id to predict goal values">>,
                    in => query,
                    schema => #{
                        type => string
                    },
                    required => true
                }
            ],
            responses => #{
                200 => #{
                    description => <<"Recommended nudges">>,
                    content => #{
                        'application/json' => #{
                            schema => nudges_return_schema()
                        }
                    }
                },
                400 => #{
                    description => <<"Bad request, see response for details">>,
                    content => #{
                        'application/json' => #{
                            schema => features_handler_v0:error_schema()
                        }
                    }
                },
                404 => features_handler_v0:not_found_spec()
            }
        }
    },
    [trails:trail("/v0/nudges", ?MODULE, #{}, Metadata)].

nudges_return_schema() ->
    #{
        type => object,
        description => <<"Nudges">>,
        properties => #{
            <<"nudges">> => #{
                type => array,
                description => <<"Recommended nudges">>
            },
            <<"user_id">> => #{
                type => string,
                description => <<"User ID used">>
            },
            <<"goal">> => #{
                type => string,
                description => <<"Goal nudging toward">>
            }
        }
    }.

init(Req, Opts) ->
    {specified_handler, Req, Opts}.

handle_req(
    Req = #{method := <<"GET">>},
    Params,
    _Body = undefined,
    State
) ->
    Namespace = proplists:get_value(namespace, Params),
    Goal = proplists:get_value(goal, Params),
    RequestedUserID = proplists:get_value(user_id, Params),
    UserEvents = determine_events_for_predictions(Namespace, RequestedUserID),
    Predictions = features_bayesian_predictor:for_goal_counts(Namespace),
    Nudges = predictions_to_nudges(Goal, UserEvents, Predictions),
    Data = #{
        <<"nudges">> => Nudges,
        <<"goal">> => Goal,
        <<"user_id">> => RequestedUserID
    },
    {Req, 200, Data, State}.

post_req(_Response, _State) ->
    ok.

%%%
%%% Internal functions
%%%

determine_events_for_predictions(Namespace, UserID) when is_binary(UserID) ->
    features_count_router:events_for_key(Namespace, UserID).

predictions_to_nudges(Goal, CompletedEvents, Predictions) ->
    GoalPredictions = maps:get(Goal, Predictions, #{}),

    % [{event, prediction value}]
    EventPredPairs = maps:to_list(GoalPredictions),

    FilterSeenEvents = fun({E, _P}) ->
        not lists:member(E, CompletedEvents)
    end,

    IncompleteEventPredictionPairs = lists:filter(FilterSeenEvents, EventPredPairs),

    SortedPredPairs = lists:sort(
        fun({_AE, AP}, {_BE, BP}) -> AP > BP end,
        IncompleteEventPredictionPairs
    ),

    Nudges = lists:map(
        fun({Name, Pred}) -> #{<<"event_name">> => Name, <<"prediction">> => Pred} end,
        SortedPredPairs
    ),

    Nudges.
