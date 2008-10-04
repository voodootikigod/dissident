%% @author Chris Williams <chris@iterativedesigns.com>
%% @copyright 2008 Iterative Designs

%% @doc Supervisor for the dissident framework application.

-module(dissident_sup).
-author('Chris Williams <chris@iterativedesigns.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

dispatch_map() ->
	% Dispatch stylesheets, javascripts, and images automatically as they are static resources
	% Handle all others using dissident's router
    [
			{["stylesheets", '*'], filesystem_resource, [{root, "priv/www/stylesheets"}]},
			{["javascripts", '*'], filesystem_resource, [{root, "priv/www/javascripts"}]},
			{["images", '*'], filesystem_resource, [{root, "priv/www/images"}]},
			{['*'], dissident_router, []}
     	
    ].

%% @doc supervisor callback.
init([]) ->
    Ip = case os:getenv("DISSIDENT_IP") of false -> "0.0.0.0"; Any -> Any end,
    Port = case os:getenv("DISSIDENT_PORT") of false -> 8080; Any1 -> Any1 end,
    WebConfig = [
		 {ip, Ip},
		 {port, Port},
         {log_dir, "priv/log"},
		 {dispatch, dispatch_map()},
		 {error_handler, dissident_error_handler}
		],
    Web = {webmachine_mochiweb,
	   {webmachine_mochiweb, start, [WebConfig]},
	   permanent, 5000, worker, dynamic},
    Processes = [Web],
    {ok, {{one_for_one, 10, 10}, Processes}}.
