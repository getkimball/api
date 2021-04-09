-module(features_handler_v0_features).

-include_lib("kernel/include/logger.hrl").

-export([trails/0]).
-export([init/2]).

-export([
    handle_req/4,
    post_req/2
]).

-callback add(binary(), binary()) -> ok.

trails() ->
    Metadata = #{
        <<"get">> => #{
            operationId => getFeatures,
            tags => ["Features"],
            description => "Gets features and their status",
            parameters => [
                #{
                    name => user_obj,
                    description =>
                        <<"User Object JSON serialized\n"
                            "                                    then Base64 encoded">>,
                    in => query,
                    schema => #{
                        type => string,
                        format => byte
                    },
                    required => false
                },
                #{
                    name => context_obj,
                    description =>
                        <<"Context Object JSON serialized\n"
                            "                                    then Base64 encoded">>,
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
                        }
                    }
                },
                400 => #{
                    description => <<"Bad request, see response for details">>,
                    content => #{
                        'application/json' => #{
                            schema => error_schema()
                        }
                    }
                }
            }
        }
    },
    {ok, Mod} = application:get_env(features, analytics_event_mod),
    State = #{analytics_event_mod => Mod},
    [trails:trail("/v0/features", ?MODULE, State, Metadata)].

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

init(Req, InitialState) ->
    % Trap exits for this process. Work will be done after the request is sent
    % and Cowboy wants to shut down the process. We don't want that though.
    % Cowboy will eventually come around and try to kill the process afterwards
    % aafter a timeout
    process_flag(trap_exit, true),
    {specified_handler, Req, InitialState}.

handle_req(Req = #{method := <<"GET">>}, Params, _Body = undefined, State) ->
    UserObj = decode_json_param(user_obj, Params),
    ContextObj = decode_json_param(context_obj, Params),
    Features = features_store:get_features(),
    CollapsedFeatures = features:collapse_features_to_map(Features, UserObj),
    ?LOG_DEBUG(#{
        what => "get features",
        context => ContextObj,
        user => UserObj,
        map => Features
    }),
    Data = #{<<"features">> => CollapsedFeatures},
    {Req, 200, Data, State#{user => UserObj, context => ContextObj}}.

post_req(
    _Response,
    _State = #{
        analytics_event_mod := AnalyticsEventMod,
        user := User,
        context := Context
    }
) ->
    ?LOG_DEBUG(#{
        what => <<"post req">>,
        mod => AnalyticsEventMod,
        user => User,
        ctx => Context
    }),
    store_feature(AnalyticsEventMod, User, Context),
    ok;
post_req(_Response, _State) ->
    ?LOG_DEBUG(#{
        what => <<"post_req but unhandled">>,
        state => _State
    }),
    ok.

store_feature(Mod, #{<<"user_id">> := UserId}, #{<<"feature">> := Feature}) ->
    ?LOG_DEBUG(#{
        what => <<"Storing feature">>,
        user => UserId,
        mod => Mod,
        feature => Feature
    }),
    Mod:add(Feature, UserId);
store_feature(Mode, User, Context) ->
    ?LOG_INFO(#{
        what => <<"store features doesn't have enough data">>,
        user => User,
        mode => Mode,
        ctx => Context
    }),
    ok.

decode_json_param(Name, Params) ->
    ObjString = proplists:get_value(Name, Params),
    case ObjString of
        undefined ->
            #{};
        JSON ->
            features_json:decode_or_throw(
                JSON,
                {invalid_json, Name}
            )
    end.
