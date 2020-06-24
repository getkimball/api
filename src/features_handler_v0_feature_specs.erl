-module(features_handler_v0_feature_specs).
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
            responses => #{
                200 => #{
                    description => <<"Features">>,
                    content => #{
                        'application/json' => #{
                            schema => features_return_schema()
                    }}

                }
            }
        },
        <<"post">> => #{
            operationId => setFeatureSpec,
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
    [trails:trail("/v0/featureSpecs", ?MODULE, [], Metadata)].

features_return_schema() ->
    #{
        type => object,
        description => <<"Feature Object">>,
        properties => #{
           <<"featureSpecs">> => #{
              description => <<"Collection of features">>,
              type => array,
              items => feature_input_schema()

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
                        },
                        #{type => object,
                          required => [property, comparator, value],
                          properties => #{
                              property => #{
                                 type => string
                              },
                              comparator => #{
                                 type => string,
                                 enum => [<<"in">>]
                              },
                              value => #{
                                 type => array,
                                 items => #{
                                     type => integer
                                }
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
                                 enum => [<<"in">>]
                              },
                              value => #{
                                 type => array,
                                 items => #{
                                     type => string
                                }
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

handle_req(Req=#{method := <<"GET">>}, _Params, _Body, Opts) ->
    InternalFeatures = features_store:get_features(),
    Features = lists:map(fun outputitize_feature/1, InternalFeatures),
    Resp = #{<<"featureSpecs">> => Features},
    {Req, 200, Resp, Opts};

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

comparator_bin_to_atom(<<"=">>) -> '=';
comparator_bin_to_atom(<<"in">>) -> 'in'.

outputitize_feature(#{name := Name,
                      boolean := Boolean,
                      rollout_start := RolloutStart,
                      rollout_end := RolloutEnd,
                      user := User}) ->
    #{<<"name">> => Name,
      <<"boolean">> => Boolean,
      <<"rollout_start">> => feature_datetime_to_rfc(RolloutStart),
      <<"rollout_end">> => feature_datetime_to_rfc(RolloutEnd),
      <<"user">> => User}.


feature_datetime_to_rfc(<<"undefined">>) ->
    undefined;
feature_datetime_to_rfc(undefined) ->
    undefined;
feature_datetime_to_rfc(Int) when is_integer(Int) ->
    DT = calendar:system_time_to_rfc3339(Int, [{offset, "z"}]),
    binary:list_to_bin(DT).
