%% @author Andy Gross <andy@basho.com> 
%% @author Justin Sheehy <justin@basho.com>
%% @copyright 2007-2008 Basho Technologies
%% Portions derived from code Copyright 2007-2008 Bob Ippolito, Mochi Media
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

-module(webmachine_request_srv).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').
-behaviour(gen_server).
-export([start_link/5]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-include("webmachine_logger.hrl").

-define(WMVSN, "0.12").
-define(QUIP, "A Very Small Sandwich").

% Maximum recv_body() length of 50MB
-define(MAX_RECV_BODY, (50*(1024*1024))).

% 120 second default idle timeout
-define(IDLE_TIMEOUT, infinity).
-record(state, {socket=undefined,
		method=undefined,
		raw_path=undefined,
		path=undefined,
		qs=undefined,
		body=undefined,
		version=undefined,
		headers=undefined,
		cookie=undefined,
		out_headers=mochiweb_headers:empty(),
		out_body=undefined,
		response_code=undefined,
		metadata=dict:new(),
		path_info=dict:new(),
		path_tokens=undefined,
		app_root=undefined,
		range=undefined,
		peer=undefined,
		log_data=#wm_log_data{}
	       }).

start_link(Socket, Method, RawPath, Version, Headers) ->
    gen_server:start_link(?MODULE, [Socket, Method, RawPath, Version, Headers], []).

init([Socket, Method, RawPath, Version, Headers]) ->
    %%process_flag(trap_exit, true),
    %% Calling get_peer() here is a little bit of an ugly way to populate the
    %% client IP address but it will do for now.
    {_, State} = get_peer(#state{socket=Socket,
				method=Method,
				raw_path=RawPath,
				version=Version,
				headers=Headers,
				out_body = <<>>,
				 response_code=500}),
    LogData = #wm_log_data{start_time=now(),
			   method=Method,
			   headers=Headers,
			   peer=State#state.peer,
			   path=RawPath,
			   version=Version,
			   response_code=404,
			   response_length=0},
    {ok, State#state{log_data=LogData}}.

handle_call(socket, _From, State) ->
    Reply = State#state.socket,
    {reply, Reply, State};
handle_call(method, _From, State) ->
    Reply = State#state.method,
    {reply, Reply, State};
handle_call(version, _From, State) ->
    Reply = State#state.version,
    {reply, Reply, State};
handle_call(raw_path, _From, State) ->
    Reply = State#state.raw_path,
    {reply, Reply, State};
handle_call(headers, _From, State) ->
    Reply = State#state.headers,
    {reply, Reply, State};
handle_call(out_headers, _From, State) ->
    Reply = State#state.out_headers,
    {reply, Reply, State};
handle_call({get_out_header, HdrName}, _From,
            State=#state{out_headers=OutHeaders}) ->
    Reply = mochiweb_headers:get_value(HdrName, OutHeaders),
    {reply, Reply, State};
handle_call(peer, _From, State) ->
    {Reply, NewState} = get_peer(State),
    {reply, Reply, NewState};
handle_call(range, _From, State) ->
    {Reply, NewState} = get_range(State),
    {reply, Reply, NewState};
handle_call(response_code, _From, State) ->
    Reply = State#state.response_code,
    {reply, Reply, State};
handle_call(path, _From, State) ->
    {Reply, NewState} = get_path(State),
    {reply, Reply, NewState};
handle_call({get_header_value, K}, _From, State) ->
    {Reply, NewState} = get_header_value(K, State),
    {reply, Reply, NewState};
handle_call({add_response_header, K, V}, _From, State) ->
    {Reply, NewState} = add_response_header(K, V, State),
    {reply, Reply, NewState};
handle_call({add_response_headers, Hdrs}, _From, State) ->
    {Reply, NewState} = add_response_headers(Hdrs, State),
    {reply, Reply, NewState};
handle_call({merge_response_headers, Hdrs}, _From, State) ->
    {Reply, NewState} = merge_response_headers(Hdrs, State),
    {reply, Reply, NewState};
handle_call({append_to_response_body, Data}, _From, State) ->
    {Reply, NewState} = append_to_response_body(Data, State),
    {reply, Reply, NewState};
handle_call({send_response, Code}, _From, State) ->

    {Reply, NewState} = 
	case Code of
	    200 ->
		    send_ok_response(Code, State);
	    _ ->
		    send_response(Code, State)
	end,
    LogData = NewState#state.log_data,
    NewLogData = LogData#wm_log_data{finish_time=now()},
    {reply, Reply, NewState#state{log_data=NewLogData}};
handle_call({serve_file, Path, DocRoot}, _From, State) ->
    {Reply, NewState} = serve_file(Path, DocRoot, State),
    {reply, Reply, NewState};
handle_call(response_body, _From, State) ->
    {Reply, NewState} = {State#state.out_body, State},
    {reply, Reply, NewState};
handle_call(has_response_body, _From, State=#state{out_body=OutBody}) ->
    Reply = case OutBody of
                undefined -> false;
                <<>> -> false;
                [] -> false;
                _ -> true
            end,
    {reply, Reply, State};
handle_call(recv_body, _From, State) ->
    case State#state.body of
	undefined ->
	    {Reply, NewState} = do_recv_body(State),
	    {reply, Reply, NewState};
	Body -> 
	    {reply, Body, State}
    end;
handle_call({get_metadata, Key}, _From, State) ->
    Reply = case dict:find(Key, State#state.metadata) of
		{ok, Value} -> Value;
		error -> undefined
	    end,
    {reply, Reply, State};
handle_call({set_metadata, Key, Value}, _From, State) ->
    NewDict = dict:store(Key, Value, State#state.metadata),
    {reply, ok, State#state{metadata=NewDict}};
handle_call({get_path_info, Key}, _From, State) ->
    Reply = case dict:find(Key, State#state.path_info) of
		{ok, Value} -> Value;
		error -> undefined
	    end,
    {reply, Reply, State};
handle_call({set_path_info, Key, Value}, _From, State) ->
    NewDict = dict:store(Key, Value, State#state.path_info),
    {reply, ok, State#state{path_info=NewDict}};
handle_call({load_path_info, PropList}, _From, State) ->
    % Loads the path-info bindings dict from scratch.
    NewDict = dict:from_list(PropList),
    {reply, ok, State#state{path_info=NewDict}};
handle_call(get_path_tokens, _From, State) ->
    {reply, State#state.path_tokens, State};
handle_call({set_path_tokens, TokenList}, _From, State) ->
    {reply, ok, State#state{path_tokens=TokenList}};
handle_call(parse_cookie, _From, State) ->
    case State#state.cookie of
	undefined ->
	    {Reply, NewState} = do_parse_cookie(State),
	    {reply, Reply, NewState};
	Cookie ->
	    {reply, Cookie, State}
    end;
handle_call(parse_qs, _From, State) ->
    case State#state.qs of
	undefined ->
	    {Reply, NewState} = do_parse_qs(State),
	    {reply, Reply, NewState};
	QS ->
	    {reply, QS, State}
    end;
handle_call(get_app_root, _From, State) ->
    {reply, State#state.app_root, State};
handle_call({set_app_root, AppRoot}, _From, State) ->
    {reply, ok, State#state{app_root=AppRoot}};
handle_call({load_dispatch_data, PathProps, PathTokens, AppRoot}, _From, State) ->
    PathInfo = dict:from_list(PathProps),
    {reply, ok, State#state{app_root=AppRoot, path_info=PathInfo, path_tokens=PathTokens}};
handle_call({recv, Length}, _From, State) ->
    Reply = recv(State, Length),
    {reply, Reply, State};
handle_call(log_data, _From, State) ->
    Reply = State#state.log_data,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_path(State) ->
    case State#state.path of 
	undefined ->
	    {Path, _, _} = mochiweb_util:urlsplit_path(State#state.raw_path),
	    {Path, State#state{path=Path}};
	Cached ->
	    {Cached, State}
    end.

get_peer(State) ->
    case State#state.peer of
	undefined ->
            Socket = State#state.socket,
	    Peer = case inet:peername(Socket) of 
		{ok, {Addr={10, _, _, _}, _Port}} ->
		    case get_header_value("x-forwarded-for", State) of
			{undefined, _} ->
			    inet_parse:ntoa(Addr);
			{Hosts, _} ->
			    string:strip(lists:last(string:tokens(Hosts, ",")))
		    end;
		{ok, {{127, 0, 0, 1}, _Port}} ->
		    case get_header_value("x-forwarded-for", State) of
			{undefined, _} ->
			    "127.0.0.1";
			{Hosts, _} ->
			    string:strip(lists:last(string:tokens(Hosts, ",")))
		    end;
		{ok, {Addr, _Port}} ->
		    inet_parse:ntoa(Addr)
            end,
            NewState = State#state{peer=Peer},
	    {Peer, NewState};
	_ ->
	    {State#state.peer, State}
    end.

get_header_value(K, State) ->
    {mochiweb_headers:get_value(K, State#state.headers), State}.

get_outheader_value(K, State) ->
    {mochiweb_headers:get_value(K, State#state.out_headers), State}.    

add_response_header(K, V, State) ->
    NewHdrs = mochiweb_headers:enter(K, V, State#state.out_headers),
    {NewHdrs, State#state{out_headers=NewHdrs}}.

add_response_headers(Hdrs, State) ->
    F = fun({K, V}, Acc) -> mochiweb_headers:enter(K, V, Acc) end,
    NewHdrs = lists:foldl(F, State#state.out_headers, Hdrs),
    {NewHdrs, State#state{out_headers=NewHdrs}}.

merge_response_headers(Hdrs, State) ->
    F = fun({K, V}, Acc) -> mochiweb_headers:insert(K, V, Acc) end,
    NewHdrs = lists:foldl(F, State#state.out_headers, Hdrs),
    {NewHdrs, State#state{out_headers=NewHdrs}}.

append_to_response_body(Data, State) ->
    case is_binary(Data) of
	true ->
	    Data0 = State#state.out_body,
	    Data1 = <<Data0/binary,Data/binary>>,
	    {ok, State#state{out_body=Data1}};
	false -> % MUST BE an iolist! else, fail.
	    append_to_response_body(iolist_to_binary(Data), State)
    end.

send(Socket, Data) ->
    case gen_tcp:send(Socket, Data) of
	ok ->
	    ok;
	_ ->
	    exit(normal)
    end.

send_ok_response(200, InitState) ->
    {Range, State} = get_range(InitState),
    case Range of
	X when X =:= undefined; X =:= fail ->
	    send_response(200, State);
	Ranges ->
	    {PartList, Size} = range_parts(State#state.out_body, Ranges),
	    case PartList of
		[] -> %% no valid ranges
		    %% could be 416, for now we'll just return 200
		    send_response(200, State);
		PartList ->
		    {RangeHeaders, RangeBody} =
			parts_to_body(PartList, State, Size),
		    ResponseHdrs1 = mochiweb_headers:enter_from_list(
				      [{"Accept-Ranges", "bytes"} |
				       RangeHeaders],
				      mochiweb_headers:make(State#state.out_headers)),
		    NewState = State#state{out_headers=ResponseHdrs1, out_body=RangeBody},
		    send_response(206, NewState)
	    end
    end.
		     

send_response(Code, State) ->
    Length = iolist_size([State#state.out_body]),
    send(State#state.socket,
	 [make_version(State#state.version), make_code(Code), <<"\r\n">> | 
         make_headers(Length, State)]),
    case State#state.method of 
	'HEAD' ->
	    ok;
	_ ->
	    send(State#state.socket, [State#state.out_body])
    end,
    InitLogData = State#state.log_data,
    FinalLogData = InitLogData#wm_log_data{response_code=Code,
					   response_length=Length},
    {ok, State#state{response_code=Code, log_data=FinalLogData}}.

%% @spec body_length(state()) -> undefined | chunked | unknown_transfer_encoding | integer()
%% @doc  Infer body length from transfer-encoding and content-length headers.
body_length(State) ->
    case get_header_value("transfer-encoding", State) of
        {undefined, _} ->
            case get_header_value("content-length", State) of
                {undefined, _} -> 
                    undefined;
                {Length, _} ->
                    list_to_integer(Length)
            end;
        {"chunked", _} -> 
            chunked;
        Unknown ->
            {unknown_transfer_encoding, Unknown}
    end.

%% @spec do_recv_body(state()) -> binary()
%% @doc Receive the body of the HTTP request (defined by Content-Length).
%%      Will only receive up to the default max-body length of 1MB.
do_recv_body(State) ->
    do_recv_body(State, ?MAX_RECV_BODY).

%% @spec do_recv_body(state(), integer()) -> {binary(), state()}
%% @doc Receive the body of the HTTP request (defined by Content-Length).
%%      Will receive up to MaxBody bytes. 
do_recv_body(State, MaxBody) ->
    case get_header_value("expect", State) of
	{"100-continue", _} ->
	    send(State#state.socket, 
		 [make_version(State#state.version), make_code(100), <<"\r\n">>]);
	_Else ->
	    ok
    end,
    Body = case body_length(State) of
               undefined ->
                   {undefined, State};
               {unknown_transfer_encoding, Unknown} -> 
                   exit({unknown_transfer_encoding, Unknown});
               0 ->
                   {<<>>, State};
               Length when is_integer(Length), Length =< MaxBody ->
                   {recv(State, Length), State};
               Length ->
                   exit({body_too_large, Length})
           end,
    Body.

%% @spec recv(state(), integer()) -> binary()
%% @doc Receive Length bytes from the client as a binary, with the default
%%      idle timeout.
recv(State, Length) ->
    recv(State, Length, ?IDLE_TIMEOUT).

%% @spec recv(state(), integer(), integer()) -> binary()
%% @doc Receive Length bytes from the client as a binary, with the given
%%      Timeout in msec.
recv(State, Length, Timeout) ->
    Socket = State#state.socket,
    case gen_tcp:recv(Socket, Length, Timeout) of
	{ok, Data} ->
	    Data;
	_R ->
	    io:format("got socket error ~p~n", [_R]),
	    exit(normal)
    end.


do_parse_cookie(State) ->
    case get_header_value("cookie", State) of
	{undefined, _} ->
	    {[], State#state{cookie=[]}};
	{Value, _} ->
	    CookieVal = mochiweb_cookies:parse_cookie(Value),
	    {CookieVal, State#state{cookie=CookieVal}}
    end.


do_parse_qs(State) ->
    {_, QueryString, _} = mochiweb_util:urlsplit_path(State#state.raw_path),
    Parsed = mochiweb_util:parse_qs(QueryString),
    {Parsed, State#state{qs=Parsed}}.

get_range(State) ->
    case get_header_value("range", State) of
	{undefined, _} ->
	    {undefined, State#state{range=undefined}};
	{RawRange, _} ->
	    Range = parse_range_request(RawRange),
	    {Range, State#state{range=Range}}
    end.

range_parts({file, IoDevice}, Ranges) ->
    Size = iodevice_size(IoDevice),
    F = fun (Spec, Acc) ->
                case range_skip_length(Spec, Size) of
                    invalid_range ->
                        Acc;
                    V ->
                        [V | Acc]
                end
        end,
    LocNums = lists:foldr(F, [], Ranges),
    {ok, Data} = file:pread(IoDevice, LocNums),
    Bodies = lists:zipwith(fun ({Skip, Length}, PartialBody) ->
                                   {Skip, Skip + Length - 1, PartialBody}
                           end,
                           LocNums, Data),
    {Bodies, Size};

range_parts(Body0, Ranges) ->
    Body = iolist_to_binary(Body0),
    Size = size(Body),
    F = fun(Spec, Acc) ->
                case range_skip_length(Spec, Size) of
                    invalid_range ->
                        Acc;
                    {Skip, Length} ->
                        <<_:Skip/binary, PartialBody:Length/binary, _/binary>> = Body,
                        [{Skip, Skip + Length - 1, PartialBody} | Acc]
                end
        end,
    {lists:foldr(F, [], Ranges), Size}.

range_skip_length(Spec, Size) ->
    case Spec of
        {none, R} when R =< Size, R >= 0 ->
            {Size - R, R};
        {none, _OutOfRange} ->
            {0, Size};
        {R, none} when R >= 0, R < Size ->
            {R, Size - R};
        {_OutOfRange, none} ->
            invalid_range;
        {Start, End} when 0 =< Start, Start =< End, End < Size ->
            {Start, End - Start + 1};
        {_OutOfRange, _End} ->
            invalid_range
    end.

parse_range_request(RawRange) when is_list(RawRange) ->
    try
        "bytes=" ++ RangeString = RawRange,
        Ranges = string:tokens(RangeString, ","),
        lists:map(fun ("-" ++ V)  ->
                          {none, list_to_integer(V)};
                      (R) ->
                          case string:tokens(R, "-") of
                              [S1, S2] ->
                                  {list_to_integer(S1), list_to_integer(S2)};
                              [S] ->
                                  {list_to_integer(S), none}
                          end
                  end,
                  Ranges)
    catch
        _:_ ->
            fail
    end.

parts_to_body([{Start, End, Body}], State, Size) ->
    %% return body for a range reponse with a single body
    ContentType = 
	case get_outheader_value("content-type", State) of
	    {undefined, _} ->
		"text/html";
	    {CT, _} ->
		CT
	end,
    HeaderList = [{"Content-Type", ContentType},
                  {"Content-Range",
                   ["bytes ",
                    make_io(Start), "-", make_io(End),
                    "/", make_io(Size)]}],
    {HeaderList, Body};
parts_to_body(BodyList, State, Size) when is_list(BodyList) ->
    %% return
    %% header Content-Type: multipart/byteranges; boundary=441934886133bdee4
    %% and multipart body
    ContentType = 
	case get_outheader_value("content-type", State) of
	    {undefined, _} ->
		"text/html";
	    {CT, _} ->
		CT
	end,
    Boundary = mochihex:to_hex(crypto:rand_bytes(8)),
    HeaderList = [{"Content-Type",
                   ["multipart/byteranges; ",
                    "boundary=", Boundary]}],
    MultiPartBody = multipart_body(BodyList, ContentType, Boundary, Size),

    {HeaderList, MultiPartBody}.

multipart_body([], _ContentType, Boundary, _Size) ->
    ["--", Boundary, "--\r\n"];
multipart_body([{Start, End, Body} | BodyList], ContentType, Boundary, Size) ->
    ["--", Boundary, "\r\n",
     "Content-Type: ", ContentType, "\r\n",
     "Content-Range: ",
         "bytes ", make_io(Start), "-", make_io(End),
             "/", make_io(Size), "\r\n\r\n",
     Body, "\r\n"
     | multipart_body(BodyList, ContentType, Boundary, Size)].

iodevice_size(IoDevice) ->
    {ok, Size} = file:position(IoDevice, eof),
    {ok, 0} = file:position(IoDevice, bof),
    Size.


serve_file(Path, DocRoot, State) ->
    FullPath = filename:join([DocRoot, Path]),
    File = case filelib:is_dir(FullPath) of
	       true ->
		   filename:join([FullPath, "index.html"]);
	       false ->
		   FullPath
	   end,
    case lists:prefix(DocRoot, File) of
	true ->
	    case file:read_file(File) of
		{ok, Binary} ->
		    ContentType = webmachine_util:guess_mime(File),
		    {_NewHdrs, NewState0} = add_response_header(
					    "Content-Type", ContentType, State),
		    {ok, NewState} = append_to_response_body(Binary, NewState0),
		    {true, NewState};
		_ ->
		    {false, State}
	    end;
	false ->
	    {false, State}
    end.

make_io(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
make_io(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
make_io(Io) when is_list(Io); is_binary(Io) ->
    Io.

make_code(X) when is_integer(X) ->
    [integer_to_list(X), [" " | httpd_util:reason_phrase(X)]];
make_code(Io) when is_list(Io); is_binary(Io) ->
    Io.

make_version({1, 0}) ->
    <<"HTTP/1.0 ">>;
make_version(_) ->
    <<"HTTP/1.1 ">>.

make_headers(Length, State) ->
    WithCL = mochiweb_headers:enter("Content-Length",integer_to_list(Length),
                             mochiweb_headers:make(State#state.out_headers)),
    ServerHeader = "MochiWeb/1.1 WebMachine/" ++ ?WMVSN ++ " (" ++ ?QUIP ++ ")",
    WithSrv = mochiweb_headers:enter("Server", ServerHeader, WithCL),
    Hdrs = case mochiweb_headers:get_value("date", WithSrv) of
	undefined ->
            mochiweb_headers:enter("Date", httpd_util:rfc1123_date(), WithSrv);
	_ ->
	    WithSrv
    end,
    F = fun({K, V}, Acc) ->
		[make_io(K), <<": ">>, V, <<"\r\n">> | Acc]
	end,
    lists:foldl(F, [<<"\r\n">>], mochiweb_headers:to_list(Hdrs)).

