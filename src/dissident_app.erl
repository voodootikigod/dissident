%% @author Chris Williams <chris@iterativedesigns.com>
%% @copyright 2008 Iterative Designs.

%% @doc Callbacks for the dissident application.

-module(dissident_app).
-author('Chris Williams <chris@iterativedesigns.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for dissident.
start(_Type, _StartArgs) ->
    dissident_deps:ensure(),
    dissident_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for dissident.
stop(_State) ->
    ok.
