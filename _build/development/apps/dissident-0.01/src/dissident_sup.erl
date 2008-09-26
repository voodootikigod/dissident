%% @author Chris Williams <chris@iterativedesigns.com>
%% @copyright 2008 Iterative Designs

%% @doc Supervisor for the dissident application.

-module(dissident_sup).
-author('chris@iterativedesigns.com').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),
    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback, ensures yaws is in embedded mode and then
%%      returns the supervisor tree.
init([]) ->
    Processes = [],
    {ok, {{one_for_one, 10, 10}, Processes}}.
