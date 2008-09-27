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
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    FullPath = filename:join([DocRoot, mochiweb_util:safe_relative_path(Path)]),
		Dir = filelib:is_dir(FullPath),
		File = filelib:is_file(FullPath),
		case File of
			true	->
				Req:serve_file(Path,DocRoot);
			_ ->
				io:format("Query for code.")
		end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
