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
        type => object,
        properties => #{
           <<"features">> => #{
              type => object,
              description => <<"Collection of features">>,
              additionalProperties => #{
                type => object,
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
               type => string,
               description => <<"name of feature">>
           },
           boolean => #{
               type => boolean,
               description => <<"Basic 'enabled' status ">>
           },
           rollout_start => #{
               type => string,
               format => 'date-time',
               description => <<"Start date-time for the rollout">>
           },
           rollout_end => #{
               type => string,
               format => 'date-time',
               description => <<"Ending date-time for the rollout">>
           }
       }
    }.

init(Req, Opts) ->
    {swagger_specified_handler, Req, Opts}.

handle_req(Req=#{method := <<"POST">>}, Params, Opts) ->
    Data = proplists:get_value(feature, Params),
    BooleanOk = handle_boolean(Data),
    RolloutOk = handle_rollout(Data),
    Code = case {BooleanOk, RolloutOk} of
        {ok, ok} -> 204;
        _ -> 405
    end,
    {Req, Code, #{}, Opts};
handle_req(Req=#{method := <<"GET">>}, _Params, Opts) ->
    Features = features_store:get_features(),
    CollapsedFeatures = features:collapse_features_map(Features),
    ?LOG_DEBUG(#{what=> "collapse map",
                 map => Features}),
    Data = #{<<"features">> => CollapsedFeatures},
    {Req, 200, Data, Opts};
handle_req(Req, _Params, Opts) ->
    {Req, 404, #{}, Opts}.


handle_boolean(#{name := FeatureName, boolean := undefined}) ->
    ?LOG_DEBUG(#{what=><<"API Set Boolean - No boolean defined">>,
                 module=>?MODULE,
                 feature_name=> FeatureName}),
    ok;
handle_boolean(#{name := FeatureName, boolean := FeatureBoolean}) ->
    FeatureStatus = case FeatureBoolean of
        <<"true">> -> true;
        <<"false">> -> false;
        true -> true;
        false -> false
    end,
    ?LOG_DEBUG(#{what=><<"API Set Boolean">>,
                 module=>?MODULE,
                 feature_name=> FeatureName,
                 feature_status=> FeatureStatus}),
    Resp = features_store:set_feature(FeatureName, boolean, FeatureStatus),
    Resp.

handle_rollout(#{name := FeatureName,
                 rollout_start := Start,
                 rollout_end := End}) ->
    Resp = features_store:set_feature(FeatureName, rollout, Start, End),
    Resp.
