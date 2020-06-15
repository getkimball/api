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
        },
        <<"post">> => #{
            operationId => setFeature,
            tags => ["Features"],
            description => "Sets a feature status",
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
                    description => <<"Feature created">>,
                    content => #{
                        'application/json' => #{}
                }},
                400 => #{
                    description => <<"Bad request, see response for details">>,
                    content => #{
                        'application/json' => #{}
                }},
                405 => #{
                    description => <<"Sidecar unable to update features">>,
                    content => #{
                        'application/json' => #{}
                }}
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
           },
           user => #{
               type => array,
               items => #{
                   type => object,
                   required => [property, comparator, value],
                   properties => #{
                       property => #{
                          type => string
                       },
                       comparator => #{
                          type => string,
                          enum => [<<"=">>]
                       },
                       value => #{
                          % This eventually should be anyOf multple types
                          type => string
                       }
                   }
             }
          }
       }
    }.

init(Req, Opts) ->
    {swagger_specified_handler, Req, Opts}.

handle_req(Req=#{method := <<"POST">>}, Params, Opts) ->
    Data = proplists:get_value(feature, Params),
    Name = maps:get(name, Data),
    Boolean =  {boolean,
                  maps:get(boolean, Data)},
    Rollout =  {rollout,
                  maps:get(rollout_start, Data),
                  maps:get(rollout_end, Data)},

    UserSpecIn = maps:get(user, Data),
    UserSpec = process_user_spec_input(UserSpecIn),
    User = {user, UserSpec},
    Ok = features_store:set_feature(Name, Boolean, Rollout, User),
    Code = case Ok of
        ok -> 204;
        _ -> 405
    end,
    {Req, Code, #{}, Opts};
handle_req(Req=#{method := <<"GET">>}, Params, Opts) ->
    UserObjString = proplists:get_value(user_obj, Params),
    UserObj = case UserObjString of
        undefined -> #{};
        JSON -> features_json:decode_or_throw(
                  JSON,
                  {invalid_json, user_obj})
    end,
    Features = features_store:get_features(),
    CollapsedFeatures = features:collapse_features_to_map(Features, UserObj),
    ?LOG_DEBUG(#{what=> "collapse map",
                 map => Features}),
    Data = #{<<"features">> => CollapsedFeatures},
    {Req, 200, Data, Opts};
handle_req(Req, _Params, Opts) ->
    {Req, 404, #{}, Opts}.



process_user_spec_input(undefined) ->
    [];
process_user_spec_input([]) ->
    [];
process_user_spec_input([#{property := Property,
                           comparator := Comparator,
                           value := Value} | T]) ->
    ComparatorAtom = comparator_bin_to_atom(Comparator),
    [[Property, ComparatorAtom, Value]|process_user_spec_input(T)].

comparator_bin_to_atom(<<"=">>) -> '='.
