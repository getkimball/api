-module(cowboy_test_helpers).

-export([read_reply/1,
         req/0]).

req() ->
    Ref = make_ref(),
    #{pid => self(),
      streamid => Ref}.

read_reply({ok, #{streamid:=StreamId}, _Opts}) ->
    receive
        {{_Pid, StreamId}, Msg} -> Msg
    after 10 ->
        error
    end.
