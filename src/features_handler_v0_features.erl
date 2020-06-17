-module(features_handler_v0_features).
-include_lib("kernel/include/logger.hrl").
-export([trails/0]).
-export([init/2]).

-export([handle_req/4]).


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
            requestBody => #{
                content => #{
                    'application/json' => #{
                        schema => feature_input_schema()
                    }
                }
            },
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
                   anyOf => [
                        #{type => object,
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
                                 type => string
                              }
                          }
                        },
                        #{type => object,
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
                                 type => integer
                              }
                          }
                        }
                    ]
             }
          }
       }
    }.

init(Req, Opts) ->
    {swagger_specified_handler, Req, Opts}.

handle_req(Req=#{method := <<"POST">>}, _Params, Body, Opts) ->
    Name = maps:get(name, Body),
    Boolean =  {boolean,
                  maps:get(boolean, Body)},
    Rollout =  {rollout,
                  maps:get(rollout_start, Body),
                  maps:get(rollout_end, Body)},

    UserSpecIn = maps:get(user, Body),
    UserSpec = process_user_spec_input(UserSpecIn),
    User = {user, UserSpec},
    Ok = features_store:set_feature(Name, Boolean, Rollout, User),
    Code = case Ok of
        ok -> 204;
        _ -> 405
    end,
    {Req, Code, #{}, Opts};
handle_req(Req=#{method := <<"GET">>}, Params, _Body=undefined, Opts) ->
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
handle_req(Req, _Params, _Body, Opts) ->
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
