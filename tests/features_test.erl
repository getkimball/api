-module(features_test).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, features).

collapse_to_boolean_with_boolean_test() ->
    load(),

    FalseSpec = test_utils:defaulted_feature_spec(
        #{boolean => false}),
    TrueSpec = test_utils:defaulted_feature_spec(
        #{boolean => true}),

    ?assertEqual(false, ?MUT:collapse_to_boolean(FalseSpec)),
    ?assertEqual(true, ?MUT:collapse_to_boolean(TrueSpec)),

    unload().


load() ->
    ok.

unload() ->
    ok.
