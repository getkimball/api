-module(features_handler_v0_features).
-include_lib("kernel/include/logger.hrl").
-export([trails/0]).
-export([init/2]).


trails() ->
    Metadata =    #{
        get => #{
            tags => ["Features"],
            description => "Gets features and their status",
            produces => ["application/json"],
            responses => #{
                <<"200">> => #{
                    description => <<"Features">>,
                    schema => feature_schema()

                }
            }
        },
        post => #{
            tags => ["Features"],
            description => "Sets a feature status",
            produces => ["application/json"],
            parameters => [
                #{name => <<"feature">>,
                  description => <<"Feature Object">>,
                  in => <<"body">>,
                  required => true,
                  schema => feature_schema()
                }
            ],
            responses => #{
                <<"200">> => #{
                    description => <<"Features">>,
                    schema => feature_schema()

                }
            }
      }
    },
    [trails:trail("/v0/features", ?MODULE, [], Metadata)].


feature_schema() ->
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
        <<"true">> -> <<"enabled">>;
        <<"false">> -> <<"disabled">>;
        true -> <<"enabled">>;
        false -> <<"disabled">>
    end,
    ?LOG_DEBUG(#{what=><<"">>,
                 module=>?MODULE,
                 post_data=>Data,
                 feature_name=> FeatureName,
                 feature_status=> FeatureStatus,
                 method=>Method}),
    ok = features_store:set_binary_feature(FeatureName, FeatureStatus),

    respond(Req1, 200, #{}, Opts);
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
