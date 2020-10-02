-module(features_counter_config_test).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_counter_config).

-define(TEST_BLOOM, test_bloom_filter).
-define(CONFIG_KEY, counters).

load() ->
    clear_bloom_filter_config(),
    ok.

unload(_) ->
    ok.

create_test_() ->
    {foreach,
     fun load/0,
     fun unload/1,
     [fun create_scalable_filter/0,
      fun create_filter_with_no_matching_config/0,
      fun create_scalable_filter_with_probability/0,
      fun create_fixed_filter/0,
      fun create_fixed_filter_with_atom/0,
      fun create_fixed_filter_with_probability/0,
      fun create_fixed_filter_with_multiple_configs/0
      ]}.

create_scalable_filter() ->
    Probability = 0.001,
    Size = 0,
    BF = ?MUT:create_bloom(<<"foo">>),
    {sbf, Probability, _, _, Size, _} = BF.

create_filter_with_no_matching_config() ->
    NumElements = 1000,
    Probability = 0.1,
    InitConfig = [
      #{pattern => "bar",
        type => fixed,
        size => NumElements,
        error_probability => Probability}],

    set_filter_initial_config(InitConfig),

    Size = 0,
    BF = ?MUT:create_bloom(<<"foo">>),
    {sbf, 0.001, _, _, Size, _} = BF.

create_scalable_filter_with_probability() ->
    NumElements = 1000,
    Probability = 0.1,
    InitConfig = [
      #{pattern => ".*",
        type => bloom_scalable,
        size => NumElements,
        error_probability => Probability}],

    set_filter_initial_config(InitConfig),

    Size = 0,
    BF = ?MUT:create_bloom(<<"foo">>),
    {sbf, Probability, _, _, Size, _} = BF.

create_fixed_filter() ->
    NumElements = 10000,
    InitConfig = [
      #{pattern => ".*",
        type => bloom_fixed_size,
        size => NumElements}],

    set_filter_initial_config(InitConfig),

    Probability = 0.001,
    MaxNumElements = 11395, % value gathered experimentally for the NumElements
    Size = 0,
    BF = ?MUT:create_bloom(<<"foo">>),
    {bloom, Probability, MaxNumElements, _, Size, _} = BF.

create_fixed_filter_with_atom() ->
    NumElements = 10000,
    InitConfig = [
      #{pattern => ".*",
        type => bloom_fixed_size,
        size => NumElements}],

    set_filter_initial_config(InitConfig),

    Probability = 0.001,
    MaxNumElements = 11395, % value gathered experimentally for the NumElements
    Size = 0,
    BF = ?MUT:create_bloom(foo),
    {bloom, Probability, MaxNumElements, _, Size, _} = BF.

create_fixed_filter_with_probability() ->
    NumElements = 100,
    Probability = 0.1,
    InitConfig = [
      #{pattern => ".*",
        type => bloom_fixed_size,
        size => NumElements,
        error_probability => Probability}],

    set_filter_initial_config(InitConfig),

    MaxNumElements = 105, % value gathered experimentally for the NumElements
    Size = 0,
    BF = ?MUT:create_bloom(<<"foo">>),
    {bloom, Probability, MaxNumElements, _, Size, _} = BF.

create_fixed_filter_with_multiple_configs() ->
    NumElements = 1000,
    Probability = 0.1,
    InitConfig = [
      #{pattern => "foo",
        type => bloom_fixed_size,
        size => NumElements,
        error_probability => Probability},
      #{pattern => ".*",
        type => bloom_scalable,
        size => NumElements,
        error_probability => 0.1}],

    set_filter_initial_config(InitConfig),

    Probability = 0.1,
    MaxNumElements = 1691, % value gathered experimentally for the NumElements
    Size = 0,
    BF = ?MUT:create_bloom(<<"foo">>),
    {bloom, Probability, MaxNumElements, _, Size, _} = BF.

validate_config_test_() ->
    {foreach,
     fun load/0,
     fun unload/1,
     [fun validate_empty_config/0,
      fun validate_valid_config/0,
      fun validate_config_missing_keys/0,
      fun validate_config_invalid_filter/0]}.

validate_empty_config() ->
    clear_bloom_filter_config(),
    ?assertEqual(ok, ?MUT:validate_config()).

validate_valid_config() ->
    Config = #{type => bloom_fixed_size,
               pattern => ".*",
               size => 10000},
    set_filter_initial_config([Config]),

    ?assertEqual(ok, ?MUT:validate_config()).

validate_config_missing_keys() ->
    Config = #{foo => <<"bar">>},
    set_filter_initial_config([Config]),

    Error = {invalid_bloom_filter_config,
             Config,
             "Config is missing required keys or size/error proability does not work"},

    ?assertThrow(Error, ?MUT:validate_config()).

validate_config_invalid_filter() ->
    Config = #{type => fixed,
               pattern => ".*",
               size => 10},
    set_filter_initial_config([Config]),

    Error = {invalid_bloom_filter_config,
             Config,
             "Config is missing required keys or size/error proability does not work"},

    ?assertThrow(Error, ?MUT:validate_config()).


set_filter_initial_config(Config) ->
    application:set_env(features, ?CONFIG_KEY, #{init => Config}).

clear_bloom_filter_config() ->
    application:unset_env(features, ?CONFIG_KEY).
