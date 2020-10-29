-module(features_counter_config).

-include_lib("kernel/include/logger.hrl").

-export([
    config_for_counter/2,
    create_bloom/1,
    validate_config/0
]).

config_for_counter(Name, Type) ->
    CounterConfig = application:get_env(features, counters, #{}),
    CounterInitConfigs = maps:get(Type, CounterConfig, []),
    FilterConfig = match_config_for_name(Name, CounterInitConfigs),
    FilterConfig.

create_bloom(Name) ->
    FilterConfig = config_for_counter(Name, init),

    ?LOG_DEBUG(#{
        what => "Creating bloom filter",
        name => Name,
        config => FilterConfig
    }),

    Filter = init_filter(FilterConfig),
    Filter.

validate_config() ->
    CounterConfig = application:get_env(features, counters, #{}),
    CounterInitConfigs = maps:get(init, CounterConfig, []),
    lists:foreach(fun validate_individual_config/1, CounterInitConfigs).

validate_individual_config(Config) ->
    ?LOG_DEBUG(#{
        what => "Validating bloom filter config",
        config => Config
    }),
    try init_filter(Config) of
        _Filter -> ok
    catch
        error:function_clause ->
            Msg =
                "Config is missing required keys or size/error " ++
                    "proability does not work",
            throw({invalid_bloom_filter_config, Config, Msg})
    end.

init_filter(undefined) ->
    default_filter();
init_filter(#{type := bloom_scalable, size := Size, error_probability := EP}) ->
    etbloom:sbf(Size, EP);
init_filter(#{type := bloom_fixed_size, size := Size, error_probability := EP}) ->
    etbloom:bloom(Size, EP);
init_filter(#{type := bloom_fixed_size, size := Size}) ->
    etbloom:bloom(Size).

match_config_for_name(_Name, []) ->
    undefined;
match_config_for_name(
    Name,
    [Config = #{pattern := Pattern} | T]
) when is_binary(Name) ->
    case re:run(Name, Pattern) of
        {match, _Captured} -> Config;
        nomatch -> match_config_for_name(Name, T)
    end.

default_filter() ->
    etbloom:sbf(100000, 0.001).
