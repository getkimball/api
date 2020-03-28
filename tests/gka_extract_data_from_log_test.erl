-module(gka_extract_data_from_log_test).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, gka_extract_data_from_log).

unique_app_log_test() ->
    Log = log(),

    App = ?MUT:app(Log),
    Expected = {[<<"namespace">>, <<"container">>],
                [<<"getkimball">>, <<"app">>]},


    ?assertEqual(Expected, App),

    ok.

log() ->
    Msg = #{<<"date">> => 1585406209.416189,
            <<"kubernetes">> =>
                 #{<<"container_hash">> =>
                       <<"getkimball/poc@sha256:3f06ffd0b48adf8455f76544dc09a54c1ae089caa7d5a51091ecebed28fc12fe">>,
                   <<"container_name">> => <<"app">>,
                   <<"docker_id">> =>
                       <<"a53efbc733ee78991fcc573fc61005d94e97a89634f73d807591e8d8b44a195d">>,
                   <<"host">> => <<"worker-pool-vh0b">>,
                   <<"labels">> => #{},
                   <<"namespace_name">> => <<"getkimball">>,
                   <<"pod_id">> => <<"21a2b857-ce70-41e4-8316-662164bae15d">>,
                   <<"pod_name">> => <<"getkimball-poc-68689bd9b5-wl6hh">>},
             <<"log">> =>
                 <<"                   <<\"namespace_name\">> => <<\"getkimball\">>,\n">>,
             <<"stream">> => <<"stdout">>,
             <<"time">> => <<"2020-03-28T14:36:49.416188617Z">>},
    Msg.
