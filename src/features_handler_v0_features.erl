-module(features_handler_v0_features).
-include_lib("kernel/include/logger.hrl").
-export([trails/0]).
-export([init/2]).

-export([handle_req/4,
         post_req/2]).


trails() ->
    Metadata =    #{
        <<"get">> => #{
            operationId => getFeatures,
            tags => ["Features"],
            description => "Gets features and their status",
            parameters => [
                #{name => user_obj,
                  description => <<"User Object JSON serialized
                                    then Base64 encoded">>,
                  in => query,
                  schema => #{
                    type => string,
                    format => byte
                  },
                  required => false
                },
                #{name => context_obj,
                  description => <<"Context Object JSON serialized
                                    then Base64 encoded">>,
                  in => query,
                  schema => #{
                    type => string,
                    format => byte
                  },
                  required => false
                }
            ],
            responses => #{
                200 => #{
                    description => <<"Features">>,
                    content => #{
                        'application/json' => #{
                            schema => features_return_schema()
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
    [trails:trail("/v0/features", ?MODULE, [], Metadata)].

features_return_schema() ->
    #{
        type => object,
        description => <<"Feature Object">>,
        properties => #{
           <<"features">> => #{
              type => object,
              additionalProperties => true,
              properties => #{},
              description => <<"Collection of features">>
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

handle_req(Req=#{method := <<"GET">>}, Params, _Body=undefined, _Opts) ->
    UserObj = decode_json_param(user_obj, Params),
    ContextObj = decode_json_param(context_obj, Params),
    Features = features_store:get_features(),
    CollapsedFeatures = features:collapse_features_to_map(Features, UserObj),
    ?LOG_DEBUG(#{what=> "collapse map",
                 map => Features}),
    Data = #{<<"features">> => CollapsedFeatures},
    {Req, 200, Data, #{user=>UserObj, context=>ContextObj}};
handle_req(Req, _Params, _Body, Opts) ->
    {Req, 404, #{}, Opts}.

post_req(_Response, _State) ->
    FeatureName = <<"foo">>,
    UserId = 44,
    features_count_router:add(FeatureName, UserId),
    ok.

decode_json_param(Name, Params) ->
    ObjString = proplists:get_value(Name, Params),
    case ObjString of
        undefined -> #{};
        JSON -> features_json:decode_or_throw(
                  JSON,
                  {invalid_json, Name})
    end.
