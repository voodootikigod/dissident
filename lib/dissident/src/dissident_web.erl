%%%----------------------------------------------------------------
%%% @author Chris Williams <chris@iterativedesigns.com>
%%% @doc
%%%		Dissident Router functionality
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

loop(Req, _DocRoot) ->
    "/" ++ Path = Req:get(path),
	case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
			
            case Path of
                _ ->
					io:format("here"),
                    Req:not_found()
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
