%%%----------------------------------------------------------------
%%% @author Chris Williams <chris@iterativedesigns.com>
%%% @doc
%%%		The Dissident Web Router that will proxy and handle traffic
%%%		based on the defined rules of the system.
%%% @end
%%% @copyright 2008 Iterative Designs
%%%----------------------------------------------------------------,

-module(dissident_web).
-author('Chris Williams <chris@iterativedesigns.com>').

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->   ?MODULE:loop(Req, DocRoot) end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
	case regexp:match(Path, "^.*json$") of
		{match,_,_} 	-> route(Req, Path);
		nomatch			-> Req:serve_file(Path,DocRoot)
	end.


%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

route(Req, Path)	->
	
	io:format("~p~n",[Path]).