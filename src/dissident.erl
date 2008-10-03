%% @author Chris Williams <chris@iterativedesigns.com>
%% @copyright 2008 Iterative Designs

%% @doc Callbacks for the dissident application.

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
%% @doc Start the webmachine_demo server.
start() ->
    ensure_started(crypto),
    ensure_started(webmachine),
    application:start(dissident).

%% @spec stop() -> ok
%% @doc Stop the webmachine_demo server.
stop() ->
    Res = application:stop(dissident),
    application:stop(webmachine),
    application:stop(crypto),
    Res.
