%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2008 Basho Technologies
%% Based on mochiweb_request.erl, which is Copyright 2007 Mochi Media, Inc.
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

%% @doc Webmachine HTTP Request Abstraction.

-module(webmachine_request, [Pid]).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').

-export([
	 method/0,
	 version/0,
	 path/0,
	 socket/0,
	 raw_path/0,
	 headers/0,
	 out_headers/0,
	 get_out_header/1,
	 has_out_header/1,
	 peer/0,
	 get_header_value/1,
	 add_response_header/2,
	 add_response_headers/1,
	 merge_response_headers/1,
	 append_to_response_body/1,
	 send_response/1,
	 response_code/0,
	 recv_body/0,
	 recv/1,
	 serve_file/2,
	 response_body/0,
	 has_response_body/0,
	 stop/0,
         do_redirect/0,
	 set_metadata/2,
	 get_metadata/1,
	 set_path_info/2,
	 get_path_info/1,
	 load_path_info/1,
	 load_dispatch_data/3,
	 get_path_tokens/0,
	 set_path_tokens/1,
	 get_app_root/0,
	 set_app_root/1,
	 parse_cookie/0,
	 get_cookie_value/1,
	 parse_qs/0,
	 get_qs_value/1,
	 get_qs_value/2,
         range/0,
	 log_data/0,
         call/1
	 ]).

-define(TIMEOUT, 60000).

call(Message) ->
    gen_server:call(Pid, Message, ?TIMEOUT).

method() ->
    call(method).

version() ->
    call(version).

path() ->
    call(path).

socket() ->
    call(socket).

raw_path() ->
    call(raw_path).

headers() ->
    call(headers).

out_headers() ->
    call(out_headers).

get_out_header(HeaderName) when is_list(HeaderName) ->
    call({get_out_header, HeaderName}).

has_out_header(HeaderName) when is_list(HeaderName) ->
    case THIS:get_out_header(HeaderName) of
        undefined -> false;
        _ -> true
    end.

has_response_body() ->
   call(has_response_body). 

peer() ->
    call(peer).

range() ->
    call(range).

response_code() ->
    call(response_code).

%% @spec parse_cookie() -> [{Key::string(), Value::string()}]
%% @doc Parse the cookie header.
parse_cookie() ->
    call(parse_cookie).

%% @spec get_cookie_value(Key::string) -> string() | undefined
%% @doc Get the value of the given cookie.
get_cookie_value(Key) ->
    proplists:get_value(Key, parse_cookie()).

%% @spec parse_qs() -> [{Key::string(), Value::string()}]
%% @doc Parse the query string of the URL.
parse_qs() ->
    call(parse_qs).

%% @spec get_qs_value(Key::string) -> string() | undefined
%% @doc Get the value of the given cookie.
get_qs_value(Key) ->
    proplists:get_value(Key, parse_qs()).

%% @spec get_qs_value(Key::string, Default::term()) -> string() | Default
%% @doc As get_qs_value/1, but supplies a default too.
get_qs_value(Key, Default) ->
    proplists:get_value(Key, parse_qs(), Default).

stop() ->
    gen_server:cast(Pid, stop).

recv_body() ->
    call(recv_body).

response_body() ->
    call(response_body).

get_header_value(K) ->
    call({get_header_value, K}).

add_response_header(K, V) ->
    call({add_response_header, K, V}).

add_response_headers(Hdrs) ->
    call({add_response_headers, Hdrs}).

merge_response_headers(Hdrs) ->
    call({merge_response_headers, Hdrs}).

append_to_response_body(Data) ->
    call({append_to_response_body, Data}).

send_response(Code) ->
    call({send_response, Code}).

serve_file(Path, DocRoot) ->
    call({serve_file, Path, DocRoot}).

get_metadata(Key) ->
    call({get_metadata, Key}).

do_redirect() ->
    set_metadata('do_redirect', true).

set_metadata(Key, Value) ->
    call({set_metadata, Key, Value}).

get_path_info(Key) ->
    call({get_path_info, Key}).

set_path_info(Key, Value) ->
    call({set_path_info, Key, Value}).

%% @spec load_path_info(PropList) -> ok
%% @doc Set the path-info dict to contain the props in PropList.
%% The old path-info dict is discarded.
load_path_info(PropList) when is_list(PropList) ->
    call({load_path_info, PropList}).

get_path_tokens() ->
    call(get_path_tokens).

set_path_tokens(TokenList) when is_list(TokenList) ->
    call({set_path_tokens, TokenList}).

get_app_root() ->
    call(get_app_root).

set_app_root(AppRoot) ->
    call({set_app_root, AppRoot}).

load_dispatch_data(Bindings, Path, AppRoot) ->
    call({load_dispatch_data, Bindings, Path, AppRoot}).

recv(Length) ->
    call({recv, Length}).

log_data() ->
    call(log_data).
