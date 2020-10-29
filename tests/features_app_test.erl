-module(features_app_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_app).

decide_store_lib_test() ->
    set_env_decide_and_assert_store_lib(
        [{s3_bucket, ""}, {gcs_bucket, ""}],
        undefined
    ),
    set_env_decide_and_assert_store_lib(
        [{s3_bucket, "default_bucket"}, {gcs_bucket, ""}],
        undefined
    ),
    set_env_decide_and_assert_store_lib(
        [{s3_bucket, ""}, {gcs_bucket, "default_bucket"}],
        undefined
    ),

    set_env_decide_and_assert_store_lib(
        [{s3_bucket, "foo"}, {gcs_bucket, ""}],
        features_store_lib_s3
    ),
    set_env_decide_and_assert_store_lib(
        [{s3_bucket, ""}, {gcs_bucket, "bar"}],
        features_store_lib_gcs
    ),

    ?assertThrow(
        {not_supported, multiple_storage_set},
        set_env_decide_and_assert_store_lib(
            [{s3_bucket, "foo"}, {gcs_bucket, "bar"}],
            features_store_lib_gcs
        )
    ).

set_env_decide_and_assert_store_lib(Sets, Expected) ->
    Set = fun({Var, Val}) ->
        application:set_env(features, Var, Val)
    end,
    lists:foreach(Set, Sets),

    Lib = ?MUT:decide_store_lib(),
    ?assertEqual(Expected, Lib).
