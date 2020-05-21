-module(features_handler_v0_features).
-include_lib("kernel/include/logger.hrl").
-export([trails/0]).
-export([init/2]).


trails() ->
    Metadata =    #{
        get => #{
            operationId => getFeatures,
            tags => ["Features"],
            description => "Gets features and their status",
            produces => ["application/json"],
            responses => #{
                <<"200">> => #{
                    description => <<"Features">>,
                    schema => features_return_schema()

                }
            }
        },
        post => #{
            operationId => setFeature,
            tags => ["Features"],
            description => "Sets a feature status",
            produces => ["application/json"],
            parameters => [
                #{name => <<"feature">>,
                  description => <<"Feature Object">>,
                  in => <<"body">>,
                  required => true,
                  schema => feature_input_schema()
                }
            ],
            responses => #{
                <<"204">> => #{
                    description => <<"Feature created">>
                },
                <<"405">> => #{
                    description => <<"Features">>
                }
            }
      }
    },
    [trails:trail("/v0/features", ?MODULE, [], Metadata)].

features_return_schema() ->
    #{
        type => <<"object">>,
        properties => #{
           name => #{
             type => <<"object">>,
             description => <<"name of feature">>,
             properties => #{
               enabled => #{
                 type => <<"boolean">>,
                 description => <<"Status of the feature">>
               }
             }
          }
        }
    }.

feature_input_schema() ->
    #{
        required => [<<"name">>],
        properties => #{
           name => #{
               type => <<"string">>,
               description => <<"name of feature">>
           },
           enabled => #{
               type => <<"boolean">>,
               description => <<"Status of the feature">>
           }
       }
    }.

init(Req=#{method := <<"POST">>=Method}, Opts) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    Data = jsx:decode(Body, [return_maps]),
    #{<<"name">>:=FeatureName, <<"enabled">>:= FeatureEnabled} = Data,
    FeatureStatus = case FeatureEnabled of
        <<"true">> -> true;
        <<"false">> -> false;
        true -> true;
        false -> false
    end,
    ?LOG_DEBUG(#{what=><<"">>,
                 module=>?MODULE,
                 post_data=>Data,
                 feature_name=> FeatureName,
                 feature_status=> FeatureStatus,
                 method=>Method}),
    Resp = features_store:set_binary_feature(FeatureName, FeatureStatus),
    Code = case Resp of
        ok -> 200;
        not_suported -> 405
    end,
    respond(Req1, Code, #{}, Opts);
init(Req=#{method := <<"GET">>}, Opts) ->
    Features = features_store:get_binary_features(),
    Data = #{<<"features">> => Features},
    respond(Req, 200, Data, Opts);
init(Req, Opts) ->
    respond(Req, 200, #{}, Opts).

respond(Req, Code, Value, Opts) ->
    Resp = cowboy_req:reply(Code, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(Value), Req),
    {ok, Resp, Opts}.
