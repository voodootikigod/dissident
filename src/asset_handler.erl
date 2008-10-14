%% @author Bryan Fink <bryan@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @author Justin Sheehy <justin@basho.com>
%% @copyright 2008 Basho Technologies, Inc.

-module(asset_handler).
-export([init/1]).
-export([allowed_methods/2,
	 resource_exists/2,
	 last_modified/2,
	 content_types_provided/2,
	 provide_content/2,
	 generate_etag/2]).

-record(context, {root,response_body=undefined,metadata=[]}).

-include_lib("kernel/include/file.hrl").
-include_lib("deps/webmachine/include/webmachine.hrl").

init(ConfigProps) ->
    {root, Root} = proplists:lookup(root, ConfigProps),
    {ok, #context{root=Root}}.
    
allowed_methods(_ReqProps, Context) ->
    {['GET'], Context}.


resource_exists(ReqProps, Context) ->
    Path = ?PATH(ReqProps),
    case dissident_util:file_exists(Context#context.root, Path) of 
	{true, _} ->
	    {true, Context};
	_ ->
            case Path of
                "p" -> {true, Context};
                _ -> {false, Context}
            end
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

content_types_provided(ReqProps, Context) ->
    CT = webmachine_util:guess_mime(?PATH(ReqProps)),
    {[{CT, provide_content}],
     Context#context{metadata=[{'content-type', CT}|Context#context.metadata]}}.

provide_content(ReqProps, Context) ->
    Path = ?PATH(ReqProps),
    case maybe_fetch_object(Context, Path) of 
	{true, NewContext} ->
	    Body = NewContext#context.response_body,
	    {Body, Context};
	{false, NewContext} ->
	    {error, NewContext}
    end.

last_modified(ReqProps, Context) ->
    {true, FullPath} = dissident_util:file_exists(Context#context.root, proplists:get_value(path, ReqProps)),
    LMod = filelib:last_modified(FullPath),
    {LMod, Context#context{metadata=[{'last-modified', httpd_util:rfc1123_date(LMod)}|Context#context.metadata]}}.

hash_body(Body) ->
    mochihex:to_hex(binary_to_list(crypto:sha(Body))).

generate_etag(ReqProps, Context) ->
    case maybe_fetch_object(Context, proplists:get_value(path, ReqProps)) of
        {true, BodyContext} ->
            ETag = hash_body(BodyContext#context.response_body),
            {ETag,
             BodyContext#context{metadata=[{etag,ETag}|
                                           BodyContext#context.metadata]}};
        _ ->
            {undefined, Context}
    end.
