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
    {ok, Path} = application:get_env(features, file_store_path),
    #state{path=Path}.

get_all(State=#state{path=Path}) ->
    {ok, DataBin} = file:read_file(Path),
    % TODO: Add pretty error handling for UX of reading file
    Data = jsx:decode(DataBin, [return_maps]),

    {Data, State}.

store(_Data, State=#state{}) ->
    State.

%%%%
%   Internal
%%%%
