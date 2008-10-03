-behaviour(webmachine_resource).
-export([start_link/1]).
-export([ping/2]).

start_link(Args) ->
    webmachine_resource:start_link(?MODULE, [Args]).

ping(_ReqProps, State) ->
    {pong, State}.

-define(REQ(RP), proplists:get_value(req, RP)).
-define(PATH(RP), proplists:get_value(path, RP)).


