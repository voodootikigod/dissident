%% @author Bryan Fink <bryan@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @author Justin Sheehy <justin@basho.com>
%% @copyright 2008 Basho Technologies, Inc.

-module(demo_fs_resource).
-export([init/1]).
-export([allowed_methods/2,
	 resource_exists/2,
	 last_modified/2,
	 content_types_provided/2,
	 content_types_accepted/2,
         delete_resource/2,
         post_is_create/2,
         create_path/2,
	 provide_content/2,
	 accept_content/2,
	 generate_etag/2]).

-record(context, {root,response_body=undefined,metadata=[]}).

-include_lib("kernel/include/file.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init(ConfigProps) ->
    {root, Root} = proplists:lookup(root, ConfigProps),
    {ok, #context{root=Root}}.
    
allowed_methods(_ReqProps, Context) ->
    {['HEAD', 'GET', 'PUT', 'DELETE', 'POST'], Context}.

file_path(Context, Name) ->
    RelName = case hd(Name) of
        "/" -> tl(Name);
        _ -> Name
    end,
    filename:join([Context#context.root, RelName]).

file_exists(Context, Name) ->
    NamePath = file_path(Context, Name),
    case filelib:is_regular(NamePath) of 
	true ->
	    {true, NamePath};
	false ->
	    false
    end.

resource_exists(ReqProps, Context) ->
    Path = ?PATH(ReqProps),
    case file_exists(Context, Path) of 
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
	    case file_exists(Context, Path) of 
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

content_types_accepted(ReqProps, Context) ->
    Req = ?REQ(ReqProps),
    CT = Req:get_header_value("content-type"),
    {[{CT, accept_content}],
     Context#context{metadata=[{'content-type', CT}|Context#context.metadata]}}.

accept_content(ReqProps, Context) ->
    Req = ?REQ(ReqProps),
    Path = ?PATH(ReqProps),
    FP = file_path(Context, Path),
    ok = filelib:ensure_dir(filename:dirname(FP)),
    case file_exists(Context, Path) of 
	{true, _} ->
            nop;
	_ ->
            LOC = "http://" ++ Req:get_header_value("host") ++ "/fs/" ++ Path,
            Req:add_response_header("Location", LOC)
    end,
    Value = Req:recv_body(),
    case file:write_file(FP, Value) of
        ok ->
            Req:append_to_response_body(Value),
            {true, Context};
        _ ->
            false
    end.    

post_is_create(_ReqProps, Context) ->
    {true, Context}.

create_path(ReqProps, Context) ->
    Req = ?REQ(ReqProps),
    case Req:get_header_value("slug") of
        undefined -> {undefined, Context};
        Slug ->
            case file_exists(Context, Slug) of
                {true, _} -> {undefined, Context};
                _ -> {Slug, Context}
            end
    end.

delete_resource(ReqProps, Context) ->
    Path = ?PATH(ReqProps),
    case file:delete(file_path(Context, Path)) of
        ok -> {true, Context};
        _ -> {false, Context}
    end.

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
    {true, FullPath} = file_exists(Context,
                                   proplists:get_value(path, ReqProps)),
    LMod = filelib:last_modified(FullPath),
    {LMod, Context#context{metadata=[{'last-modified',
                    httpd_util:rfc1123_date(LMod)}|Context#context.metadata]}}.

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
