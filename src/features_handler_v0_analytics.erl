-module(features_handler_v0_analytics).
-include_lib("kernel/include/logger.hrl").
-export([trails/0]).
-export([init/2]).

-export([handle_req/4,
         post_req/2]).

-callback add(binary(), binary()) -> ok.

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
                            schema => error_schema(),
                            example => #{
                                event_name => <<"click">>,
                                user_id => <<"1">>
                            }
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
    {ok, Mod} = application:get_env(features, analytics_event_mod),
    State = #{analytics_event_mod => Mod},
    [trails:trail("/v0/analytics", ?MODULE, State, Metadata)].

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
    EventSchema = #{
        type => object,
        required => [event_name, user_id],
        properties => #{
           ensure_goal => #{
               type => boolean,
               description => <<"Ensure this is a goal counter">>
           },
           event_name => #{
               type => string,
               description => <<"Name of event">>
           },
           user_id => #{
               type => string,
               description => <<"ID of the user">>
           }
        }
    },
    #{
        oneOf => [
          EventSchema,
          #{type => object,
            required => [events],
            properties => #{
               events => #{
                   type => array,
                   description => <<"List of events">>,
                   items => EventSchema
               }
            }
          }
        ]
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
    RenderedCounts = lists:map(fun render_count_map/1, Counts),
    Data = #{<<"counts">> => RenderedCounts},
    ?LOG_DEBUG(#{what=> "Analytic counts",
                 counts => RenderedCounts}),
    {Req, 200, Data, #{}};

handle_req(Req=#{method := <<"POST">>},
           _Params,
           _Body=#{events:=Events},
           State=#{analytics_event_mod:=AnalyticsEventMod}) ->

    ?LOG_DEBUG(#{what=> "Analytic events",
                 events => Events}),
    EventCalls = lists:map(fun build_event_call/1, Events),

    AnalyticsEventMod:add(EventCalls),

    {Req, 204, <<"">>, State};
handle_req(Req=#{method := <<"POST">>},
           _Params,
           Body,
           State=#{analytics_event_mod:=AnalyticsEventMod}) ->

    {EventName, UserID, Opts} = build_event_call(Body),

    ?LOG_DEBUG(#{what=> "Analytic event",
                 user_id => UserID,
                 mode => AnalyticsEventMod,
                 event_name => EventName}),


    AnalyticsEventMod:add(EventName, UserID, Opts),

    {Req, 204, <<"">>, State};
handle_req(Req, Params, Body, State) ->
    ?LOG_DEBUG(#{what => "Analytics request 404",
                 req => Req,
                 params => Params,
                 body => Body,
                 state => State}),

    {Req, 404, #{}, #{}}.

post_req(_Response, _State) ->
    ok.


render_count_map(#{name:=Name,
                   count:=Count,
                   tag_counts:=TagCounts}) ->
    RenderedTagCounts = render_tag_counts(TagCounts),
    #{name=>Name,
      count=>Count,
      event_counts=>RenderedTagCounts}.

render_tag_counts(TagCounts) ->
    maps:fold(fun render_tag_count/3, [], TagCounts).

render_tag_count(Tags, Count, AccIn) ->
    [#{events => Tags,
      count => Count}|AccIn].


build_event_call(#{ensure_goal := EnsureGoalArg,
                   event_name := EventName,
                   user_id := UserID}) ->
    EnsureGoal = case EnsureGoalArg of
        undefined -> false;
        Else -> Else
    end,
    Opts = #{
      ensure_goal => EnsureGoal
    },
    {EventName, UserID, Opts}.
