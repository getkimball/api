-module(features_handler_v0_goals).
-include_lib("kernel/include/logger.hrl").
-export([trails/0]).
-export([init/2]).

-export([handle_req/4,
         post_req/2]).

trails() ->
    Metadata =    #{
        <<"get">> => #{
            operationId => getGoals,
            tags => ["Analytics"],
            description => "Gets event goals",
            responses => #{
                200 => #{
                    description => <<"Event Goals">>,
                    content => #{
                        'application/json' => #{
                            schema => goals_return_schema()
                    }}

                },
                400 => #{
                    description => <<"Bad request, see response for details">>,
                    content => #{
                        'application/json' => #{
                            schema => error_schema()
                    }}
                }
            }
        },
        <<"post">> => #{
            operationId => addGoalEvent,
            tags => ["Analytics"],
            description => "Add an goal event",
            requestBody => #{
                content => #{
                    'application/json' => #{
                        schema => goal_event_input_schema()
                    }
                }
            },
            responses => #{
                204 => #{
                    description => <<"Goal event added">>,
                    content => #{
                        'application/json' => #{}
                }}
            }
        }
    },
    State = #{},
    [trails:trail("/v0/goals", ?MODULE, State, Metadata)].

goals_return_schema() ->
    #{
        type => object,
        description => <<"Goal events">>,
        properties => #{
           <<"goals">> => #{
              type => array,
              additionalProperties => true,
              properties => #{},
              description => <<"Collection of feature usage counts">>
           }
        }
    }.

goal_event_input_schema() ->
    #{
        required => [goal_name],
        properties => #{
           goal_name => #{
               type => string,
               description => <<"Name of goal event">>
           }
        }
    }.

error_schema() ->
    #{
        type => object,
        properties => #{
           <<"error">> => #{
              type => object,
              description => <<"Object describing the error">>
           }
        }
    }.

init(Req, Opts) ->
    {swagger_specified_handler, Req, Opts}.

handle_req(Req=#{method := <<"GET">>}, _Params, _Body=undefined, _Opts) ->
    Goals = features_count_router:goals(),
    Data = #{<<"goals">> => Goals},
    ?LOG_DEBUG(#{what=> "Goal Events",
                 goals => Goals}),
    {Req, 200, Data, #{}};
handle_req(Req=#{method := <<"POST">>},
           _Params,
           _Body=#{goal_name:= GoalName},
           State) ->

    ?LOG_DEBUG(#{what=> "Goal event",
                 goal_name => GoalName}),
    features_count_router:add_goal(GoalName),

    {Req, 204, <<"">>, State};
handle_req(Req, Params, Body, State) ->
    ?LOG_DEBUG(#{what => "Goal event request 404",
                 req => Req,
                 params => Params,
                 body => Body,
                 state => State}),

    {Req, 404, #{}, #{}}.

post_req(_Response, _State) ->
    ok.
