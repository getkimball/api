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
    ReadFile = file:read_file(Path),
    DataBin = handle_file(ReadFile, Path),
    Data = jsx:decode(DataBin, [return_maps]),

    {Data, State}.

handle_file({ok, DataBin}, _Path) ->
    DataBin;
handle_file({error, enoent}, Path) ->
    throw({enoent, Path}).

store(_Data, State=#state{}) ->
    {not_suported, State}.

%%%%
%   Internal
%%%%
