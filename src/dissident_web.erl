%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for dissident.

-module(dissident_web).
-author('author <author@example.com>').

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
	case filelib:is_dir(FullPath)


	case  of
        undefined	->
			io:format("query methods");
		RelPath ->
			io:format("~p~n",[RelPath]),
			Req:serve_file(Path,DocRoot)
	end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
