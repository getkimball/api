-module(cb_auth_simple_header).

-export([execute/2]).

execute(Req, Env=#{cb_auth := #{auth_tokens := Tokens,
                                unauth_handler := UnauthHandler}}) ->
    AuthHeader = cowboy_req:header(<<"Authorization">>, Req),
    ErrorEnv = Env#{handler => UnauthHandler},

    NewEnv = case token_for_authheader(AuthHeader) of
        undefined -> ErrorEnv;
        TokenValue -> case lists:member(TokenValue, Tokens) of
            true -> Env;
            false -> ErrorEnv
        end
    end,
    {ok, Req, NewEnv}.

token_for_authheader(undefined) ->
    undefined;
token_for_authheader(<< _Bearer:7/binary, Token/binary >>) ->
    % 7 bytes for "bearer "
    Token.

