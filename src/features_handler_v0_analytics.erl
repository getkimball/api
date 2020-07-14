-module(features_handler_v0_analytics).
-include_lib("kernel/include/logger.hrl").
-export([trails/0]).
-export([init/2]).

-export([handle_req/4,
         post_req/2]).


trails() ->
    Metadata =    #{
        <<"get">> => #{
            operationId => getAnalytics,
            tags => ["Analytics"],
            description => "Gets features analytics",
            responses => #{
                200 => #{
                    description => <<"Features">>,
                    content => #{
                        'application/json' => #{
                            schema => analytics_return_schema()
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
            operationId => addAnalyticEvent,
            tags => ["Analytics"],
            description => "Add an analytic event with user and feature",
            requestBody => #{
                content => #{
                    'application/json' => #{
                        schema => analytic_event_input_schema()
                    }
                }
            },
            responses => #{
                204 => #{
                    description => <<"Analytic event added">>,
                    content => #{
                        'application/json' => #{}
                }}
            }
        }
    },
    [trails:trail("/v0/analytics", ?MODULE, [], Metadata)].

analytics_return_schema() ->
    #{
        type => object,
        description => <<"Analytics object">>,
        properties => #{
           <<"counts">> => #{
              type => object,
              additionalProperties => true,
              properties => #{},
              description => <<"Collection of feature usage counts">>
           }
        }
    }.

analytic_event_input_schema() ->
    #{
        required => [feature_name, user_id],
        properties => #{
           feature_name => #{
               type => string,
               description => <<"Name of feature">>
           },
           user_id => #{
               type => string,
               description => <<"ID of the user">>
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
    Counts = features_count_router:counts(),
    Data = #{<<"counts">> => Counts},
    ?LOG_DEBUG(#{what=> "Analytic counts",
                 counts => Counts}),
    {Req, 200, Data, #{}};
handle_req(Req=#{method := <<"POST">>},
           _Params,
           _Body=#{feature_name:= FeatureName,
                   user_id:= UserID},
           _Opts) ->

    ?LOG_DEBUG(#{what=> "Analytic event",
                 user_id => UserID,
                 feature_name => FeatureName}),
    features_count_router:add(FeatureName, UserID),

    {Req, 204, <<"">>, #{}};
handle_req(Req, _Params, _Body, _Opts) ->
    {Req, 404, #{}, #{}}.

post_req(_Response, _State) ->
    ok.
