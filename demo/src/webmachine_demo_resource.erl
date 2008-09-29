%% @author Andy Gross <andy@basho.com> 
%% @author Justin Sheehy <justin@basho.com>
%% @copyright 2007-2008 Basho Technologies, Inc.  All Rights Reserved.
%% @doc Example webmachine_resource.

-module(webmachine_demo_resource).
-author('Andy Gross <andy@basho.com>').
-author('Justin Sheehy <justin@basho.com>').
-export([init/1, to_html/2, to_text/2, content_types_provided/2,
         is_authorized/2, generate_etag/2, expires/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.
    
content_types_provided(_ReqProps, Context) ->
    {[{"text/html", to_html},{"text/plain",to_text}], Context}.

to_text(ReqProps, Context) ->
    Path = ?PATH(ReqProps),
    Body = io_lib:format("Hello ~s from webmachine.~n", [Path]),
    {Body, Context}.

to_html(ReqProps, Context) ->
    {Body, Ctx2} = to_text(ReqProps, Context),
    HBody = io_lib:format("<html><body>~s</body></html>~n",
                          [erlang:iolist_to_binary(Body)]),
    {HBody, Ctx2}.

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
                            {"Basic realm=webmachine", Context}
                    end;
                _ ->
                    {"Basic realm=webmachine", Context}
            end;
        _ -> {true, Context}
    end.

expires(_ReqProps, Context) ->
    {{{2009,1,1},{0,0,0}}, Context}.

generate_etag(ReqProps, Context) ->
    {?PATH(ReqProps), Context}.
