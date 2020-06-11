-module(features_json).

-export([decode_or_throw/2]).

decode_or_throw(Bin, Throw) ->
    case jsx:is_json(Bin) of
        true -> jsx:decode(Bin, [return_maps]);
        false -> throw(Throw)
    end.
