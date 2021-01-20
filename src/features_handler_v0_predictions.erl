-module(features_handler_v0_predictions).

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
                    name => event,
                    description =>
                        <<"User event to predict goal values">>,
                    in => query,
                    schema => #{
                        type => array,
                        items => #{
                            type => string
                        }
                    },
                    required => false
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
                    required => false
                }
            ],
            responses => #{
                200 => #{
                    description => <<"Bayesian predictions">>,
                    content => #{
                        'application/json' => #{
                            schema => predictions_return_schema()
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
    [trails:trail("/v0/predictions", ?MODULE, #{}, Metadata)].

predictions_return_schema() ->
    #{
        type => object,
        description => <<"Predictions object">>,
        properties => #{
            <<"goals">> => #{
                type => object,
                additionalProperties => true,
                properties => #{},
                description => <<"Collection of predictions">>
            }
        }
    }.

init(Req, Opts) ->
    {swagger_specified_handler, Req, Opts}.

handle_req(
    Req = #{method := <<"GET">>},
    Params,
    _Body = undefined,
    State
) ->
    Namespace = proplists:get_value(namespace, Params),
    RequestedEvents = proplists:get_value(event, Params),
    RequestedUserID = proplists:get_value(user_id, Params),
    Events = determine_events_for_predictions(Namespace, RequestedUserID, RequestedEvents),
    Resp =
        try get_predictions(Namespace, Events) of
            Predictions ->
                Data = #{<<"goals">> => Predictions},
                {Req, 200, Data, State}
        catch
            {unknown_event, Event} ->
                Response = #{
                    error => #{
                        what => <<"Event is not known in this namespace">>,
                        event => Event
                    }
                },
                {Req, 400, Response, State}
        end,
    Resp.

post_req(_Response, _State) ->
    ok.

%%%
%%% Internal functions
%%%

determine_events_for_predictions(Namespace, UserID, []) when is_binary(UserID) ->
    Events = features_count_router:events_for_key(Namespace, UserID),
    Events;
determine_events_for_predictions(_Namespace, _UserID, Events) ->
    Events.

get_predictions(Namespace, []) ->
    Predictions = features_bayesian_predictor:for_goal_counts(Namespace),
    RP = maps:map(
        fun render_bayes_as_predictions/2,
        Predictions
    ),
    RP;
get_predictions(Namespace, Events) ->
    Predictions = features_bayesian_predictor:for_events(Namespace, Events),
    Predictions.

render_bayes_as_predictions(_K, V) ->
    RenderedEvents = maps:map(fun render_events_to_bayes_maps/2, V),
    #{<<"events">> => RenderedEvents}.

render_events_to_bayes_maps(_K, V) ->
    #{<<"bayes">> => V}.
