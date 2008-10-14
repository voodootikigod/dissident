%% @author Chris Williams <chris@iterativedesigns.com>
%% @copyright 2008 Iterative Designs

%% @doc Router for the dissident framework application.

-module(dissident_router).
-author('Chris Williams <chris@iterativedesigns.com>').

-export([init/1, content_types_provided/2, allowed_methods/2, last_modified/2,
         is_authorized/2, generate_etag/2, expires/2]).

-record(context, {root,response_body=undefined,metadata=[]}).
-include_lib("deps/webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.
    
content_types_provided(_ReqProps, Context) ->
    {[{"text/html", render_file}, {"application/x-javascript",route_to_handler}, {"application/json",route_to_handler}], Context}.

	
allowed_methods(_ReqProps, Context) ->
    {['HEAD', 'GET', 'PUT', 'DELETE', 'POST'], Context}.


% to_text(ReqProps, Context) ->
%     Path = ?PATH(ReqProps),
%     Body = io_lib:format("Hello ~s from dissident.~n", [Path]),
%     {Body, Context}.


route_to_handler(ReqProps, Context)	->
	Path = ?PATH(ReqProps),
    Body = io_lib:format("JSON attempted: ~s.~n", [Path]),
    {Body, Context}.
	

render_file(ReqProps, Context)	->
    Path = ?PATH(ReqProps),
    case maybe_fetch_object(Context, Path) of 
	{true, NewContext} ->
	    Body = NewContext#context.response_body,
	    {Body, Context};
	{false, NewContext} ->
	    {error, NewContext}
    end.


maybe_fetch_object(Context, Path) ->
    % if returns {true, NewContext} then NewContext has response_body
    case Context#context.response_body of
	undefined ->
	    case dissident_util:file_exists(Context#context.root, Path) of 
		{true, FullPath} ->
		    {ok, Value} = file:read_file(FullPath),
		    {true, Context#context{response_body=Value}};
		false ->
		    {false, Context}
	    end;
	_Body ->
	    {true, Context}
    end.

is_authorized(ReqProps, Context) ->
    Req = ?REQ(ReqProps),
    case ?PATH(ReqProps) of
        "authdemo" -> 
            case Req:get_header_value("authorization") of
                "Basic "++Base64 ->
                    Str = base64:mime_decode_to_string(Base64),
                    case string:tokens(Str, ":") of
                        ["authdemo", "demo1"] ->
                            {true, Context};
                        _ ->
                            {"Basic realm=dissident", Context}
                    end;
                _ ->
                    {"Basic realm=dissident", Context}
            end;
        _ -> {true, Context}
    end.

expires(_ReqProps, Context) ->
    {{{2009,1,1},{0,0,0}}, Context}.


generate_etag(ReqProps, Context) ->
    {?PATH(ReqProps), Context}.

last_modified(ReqProps, Context) ->
    {true, FullPath} = dissident_util:file_exists(Context#context.root, proplists:get_value(path, ReqProps)),
    LMod = filelib:last_modified(FullPath),
    {LMod, Context#context{metadata=[{'last-modified', httpd_util:rfc1123_date(LMod)}|Context#context.metadata]}}.
