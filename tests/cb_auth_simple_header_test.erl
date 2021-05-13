-module(cb_auth_simple_header_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, cb_auth_simple_header).
-define(CTH, cowboy_test_helpers).

load() ->
    ok.

unload(_) ->
    ok.

bearer_auth_test_() ->
    {foreach, fun load/0, fun unload/1, [
        fun bearer/0,
        fun bearer_no_header/0,
        fun bearer_no_tokens/0,
        fun exclude_handler/0
    ]}.

bearer() ->
    Token = <<"foo">>,
    Headers = #{
        <<"Authorization">> => <<<<"Bearer ">>/binary, Token/binary>>
    },
    Req = ?CTH:req(<<"GET">>, #{headers => Headers}),

    Env = #{
        cb_auth => #{
            auth_tokens => [Token],
            unauth_handler => unauth_handler
        },
        handler => test_handler
    },

    MiddlewareResp = ?MUT:execute(Req, Env),

    ?assertEqual({ok, Req, Env}, MiddlewareResp).

bearer_no_header() ->
    Token = <<"foo">>,
    Headers = #{},
    Req = ?CTH:req(<<"GET">>, #{headers => Headers}),

    Env = #{
        cb_auth => #{
            auth_tokens => [Token],
            unauth_handler => unauth_handler
        },
        handler => test_handler
    },

    MiddlewareResp = ?MUT:execute(Req, Env),

    ExpectedEnv = Env#{handler => unauth_handler},

    ?assertEqual({ok, Req, ExpectedEnv}, MiddlewareResp).

bearer_no_tokens() ->
    Token = <<"foo">>,
    Headers = #{
        <<"Authorization">> => <<<<"Bearer ">>/binary, Token/binary>>
    },
    Req = ?CTH:req(<<"GET">>, #{headers => Headers}),

    Env = #{
        cb_auth => #{
            auth_tokens => [],
            unauth_handler => unauth_handler
        },
        handler => test_handler
    },

    MiddlewareResp = ?MUT:execute(Req, Env),

    ExpectedEnv = Env#{handler => unauth_handler},

    ?assertEqual({ok, Req, ExpectedEnv}, MiddlewareResp).

exclude_handler() ->
    Token = <<"foo">>,
    Headers = #{},
    Req = ?CTH:req(<<"GET">>, #{headers => Headers}),
    Handler = test_handler,

    Env = #{
        cb_auth => #{
            auth_tokens => [Token],
            exclude_handlers => [Handler],
            unauth_handler => unauth_handler
        },
        handler => Handler
    },

    MiddlewareResp = ?MUT:execute(Req, Env),

    ExpectedEnv = Env#{handler => Handler},

    ?assertEqual({ok, Req, ExpectedEnv}, MiddlewareResp).
