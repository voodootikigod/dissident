%%%----------------------------------------------------------------
%%% @author Chris Williams <chris@iterativedesigns.com>
%%% @doc
%%%		The Dissident Web Application Server.
%%% @end
%%% @copyright 2008 Iterative Designs
%%%----------------------------------------------------------------,

-module(dissident).
-author('Chris Williams <chris@iterativedesigns.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the dissident server.
start() ->
    dissident_deps:ensure(),
    ensure_started(crypto),
    application:start(dissident).

%% @spec stop() -> ok
%% @doc Stop the dissident server.
stop() ->
    Res = application:stop(dissident),
    application:stop(crypto),
    Res.
