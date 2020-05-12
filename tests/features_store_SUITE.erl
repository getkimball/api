-module(features_store_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").


-define(MUT, features_store).


all() -> [{group, test_ets}].

groups() -> [{test_ets, [
                aa_write_read
              ]}
            ].


init_per_testcase(_, Config) ->
    {ok, Pid} = ?MUT:start_link(),

    [{pid, Pid} | Config].

end_per_testcase(_, Config) ->
    Pid = proplists:get_value(pid, Config),
    exit(Pid, normal),
    Config.

aa_write_read(_Config) ->
    Name = <<"feature">>,
    Status = <<"enabled">>,

    ok = features_store:set_binary_feature(Name, Status),
    Resp = features_store:get_binary_features(),

    Expected = #{Name => enabled},
    ?assertEqual(Expected, Resp),

    ok.
