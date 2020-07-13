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
handle_req(Req, _Params, _Body, _Opts) ->
    {Req, 404, #{}, #{}}.

post_req(_Response, _State) ->
    ok.
