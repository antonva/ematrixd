-module(client_server_version_handler).
-compile(export_all).


init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, json_version}
     ], Req, State}.

json_version(Req, State) ->
    Body = <<"{\"implement\": \"me\"}">>,
    {Body, Req, State}.
