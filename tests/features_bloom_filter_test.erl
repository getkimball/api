-module(features_bloom_filter_test).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_bloom_filter).

-define(TEST_BLOOM, test_bloom_filter).

load() ->
    ok.

unload(_) ->
    ok.

create_test_() ->
    {foreach,
     fun load/0,
     fun unload/1,
     [fun create_scalable_filter/0]}.

create_scalable_filter() ->
    Probability = 0.001,
    Size = 0,
    BF = ?MUT:create(foo),
    {sbf, Probability, _, _, Size, _} = BF.
