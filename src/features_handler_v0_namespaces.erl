-module(features_handler_v0_namespaces).

-include_lib("kernel/include/logger.hrl").

-export([trails/0]).
-export([init/2]).

-export([
    handle_req/4,
    post_req/2
]).

trails() ->
    Metadata = #{
        <<"get">> => #{
            operationId => getGoals,
            tags => ["Analytics"],
            description => "Gets event namespaces",
            responses => #{
                200 => #{
                    description => <<"Known namespaces">>,
                    content => #{
                        'application/json' => #{
                            schema => namespacess_return_schema()
                        }
                    }
                },
                400 => #{
                    description => <<"Bad request, see response for details">>,
                    content => #{
                        'application/json' => #{
                            schema => features_handler_v0:error_schema()
                        }
                    }
                },
                404 => features_handler_v0:not_found_spec()
            }
        }
    },
    State = #{},
    [trails:trail("/v0/namespaces", ?MODULE, State, Metadata)].

namespacess_return_schema() ->
    #{
        type => object,
        description => <<"Namespaces">>,
        properties => #{
            <<"namespaces">> => #{
                type => array,
                additionalProperties => true,
                properties => #{},
                description => <<"List of namespaces">>
            }
        }
    }.


init(Req, Opts) ->
    {swagger_specified_handler, Req, Opts}.

handle_req(
    Req = #{method := <<"GET">>},
    _Params,
    _Body = undefined,
    _State
) ->
    Namespaces = features_count_router:namespaces(),
    Data = #{<<"namespaces">> => Namespaces},
    {Req, 200, Data, #{}}.

post_req(_Response, _State) ->
    ok.
