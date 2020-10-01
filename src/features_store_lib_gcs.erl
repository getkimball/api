-module(features_store_lib_gcs).
-include_lib("kernel/include/logger.hrl").
-include_lib("enenra/include/enenra.hrl").
-behaviour(features_store_lib).
-export([init/1,
         get_all/1,
         store/2]).

-record(state, {bucket=undefined,
                creds=undefined,
                path=undefined}).


%%%%
%   features_store api
%%%%

init(Name) ->
    {ok, Bucket} = application:get_env(features, gcs_bucket),
    {ok, BasePath} = application:get_env(features, gcs_base_path),
    {ok, GCSCP} = application:get_env(features, gcs_credentials_path),

    {ok, Creds} = enenra:load_credentials(GCSCP),

    BinBucket = ensure_bin(Bucket),
    Path = ensure_bin(features_store_lib_lib:name_to_path(BasePath, Name)),
    #state{bucket=BinBucket,
           creds=Creds,
           path=Path}.

ensure_bin(I) when is_binary(I) ->
    I;
ensure_bin(I) when is_list(I) ->
    binary:list_to_bin(I).

get_all(State=#state{bucket=Bucket, path=Path, creds=Creds}) ->
    Obj = case enenra:get_object_contents(Bucket, Path, Creds) of
        {ok, Returned} -> erlang:binary_to_term(Returned);
        {error, not_found} -> #{};
        {error, Reason} -> throw({enenra, Reason})
    end,
    Data = Obj,
    {Data, State}.

store(Data, State=#state{bucket=Bucket, path=Path, creds=Creds}) ->
    Encoded = erlang:term_to_binary(Data),
    Hash = base64:encode(erlang:md5(Encoded)),
    Size = size(Encoded),
    EObj = #object{bucket=Bucket,
                   id=Path,
                   contentType= <<"application/erlang">>,
                   name=Path,
                   md5Hash=Hash,
                   size=Size,
                   storageClass= <<"STANDARD">>,
                   timeCreated= <<"1970-01-01:00:00.00Z">>,
                   updated= <<"1970-01-01:00:00.00Z">>
                   },
    {ok, _EObj2} = enenra:upload_data(Encoded, EObj, Creds),
    {ok, State}.
