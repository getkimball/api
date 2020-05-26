-module(features_handler_v0_features).
-include_lib("kernel/include/logger.hrl").
-export([trails/0]).
-export([init/2]).

-export([handle_req/3]).


trails() ->
    Metadata =    #{
        <<"get">> => #{
            operationId => getFeatures,
            tags => ["Features"],
            description => "Gets features and their status",
            produces => ["application/json"],
            responses => #{
                200 => #{
                    description => <<"Features">>,
                    schema => features_return_schema()

                }
            }
        },
        <<"post">> => #{
            operationId => setFeature,
            tags => ["Features"],
            description => "Sets a feature status",
            produces => ["application/json"],
            parameters => [
                #{name => feature,
                  description => <<"Feature Object">>,
                  in => body,
                  required => true,
                  schema => feature_input_schema()
                }
            ],
            responses => #{
                204 => #{
                    description => <<"Feature created">>
                },
                405 => #{
                    description => <<"Sidecar unable to update features">>
                }
            }
      }
    },
    [trails:trail("/v0/features", ?MODULE, [], Metadata)].

features_return_schema() ->
    #{
        type => <<"object">>,
        properties => #{
           <<"features">> => #{
              type => <<"object">>,
              description => <<"Collection of features">>,
              additionalProperties => #{
                type => <<"object">>,
                description => <<"Maps of feature name to bool enabled status">>
              }
           }
        }
    }.

feature_input_schema() ->
    #{
        required => [name],
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

init(Req, Opts) ->
    {swagger_specified_handler, Req, Opts}.

handle_req(Req=#{method := <<"POST">>=Method}, Params, Opts) ->
    Data = proplists:get_value(feature, Params),
    #{name := FeatureName,
      enabled := FeatureBoolean} = Data,
    FeatureStatus = case FeatureBoolean of
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
    Resp = features_store:set_feature(FeatureName, boolean, FeatureStatus),
    Code = case Resp of
        ok -> 204;
        not_suported -> 405
    end,
    {Req, Code, #{}, Opts};
handle_req(Req=#{method := <<"GET">>}, _Params, Opts) ->
    Features = features_store:get_features(),
    CollapsedFeatures = features:collapse_features_map(Features),
    Data = #{<<"features">> => CollapsedFeatures},
    {Req, 200, Data, Opts};
handle_req(Req, _Params, Opts) ->
    {Req, 404, #{}, Opts}.
