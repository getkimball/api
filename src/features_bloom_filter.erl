-module(features_bloom_filter).

-export([create/1]).


create(_Name) ->
    etbloom:sbf(100000, 0.001).
