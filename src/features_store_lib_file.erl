-module(features_store_lib_file).
-include_lib("kernel/include/logger.hrl").
-behaviour(features_store).
-export([init/0,
         get_all/1,
         store/2]).

-record(state, {path=undefined}).

%%%%
%   features_store api
%%%%

init() ->
    Path = <<"/features/data">>,
    #state{path=Path}.

get_all(State=#state{path=Path}) ->
    {ok, DataBin} = file:read_file(Path),
    % TODO: Add pretty error handling for UX of reading file
    Data = jsx:decode(DataBin, [return_maps]),

    {Data, State}.

store(_Data, _State=#state{}) ->
    throw("Storing is not supported for this store").


%%%%
%   Internal
%%%%
