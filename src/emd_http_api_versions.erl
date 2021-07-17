-module(emd_http_api_versions).
-compile(export_all).


init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, json_version}
     ], Req, State}.

json_version(Req, State) ->
    %% TODO: store the actual versions implemented somewhere else and fetch them
    Versions = [<<"r0.5.0">>, <<"r0.6.0">>],
    UnstableFeatures = [],
    Body = jiffy:encode({[{versions, Versions}, {unstable_features, UnstableFeatures}]}),
    {Body, Req, State}.
