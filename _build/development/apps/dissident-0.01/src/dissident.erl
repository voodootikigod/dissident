%% @author Chris Williams <chris@iterativedesigns.com>
%% @copyright 2008 Iterative Designs

%% @doc Start and stop the dissident server.

-module(dissident).
-author('chris@iterativedesigns.com').

-export([start/0, stop/0]).
-export([new_request/1, new_response/1]).
-export([all_loaded/0, all_loaded/1, reload/0]).
-export([test/0]).

%% @spec start() -> ok
%% @doc Start the dissident server.
start() ->
    ensure_started(crypto),
    application:start(dissident).

%% @spec stop() -> ok
%% @doc Stop the dissident server.
stop() ->
    Res = application:stop(dissident),
    application:stop(crypto),
    Res.

%% @spec test() -> ok
%% @doc Run all of the tests for dissident.
test() ->
    dissident_util:test(),
    dissident_headers:test(),
    dissident_cookies:test(),
    mochihex:test(),
    mochinum:test(),
    mochijson:test(),
    dissident_charref:test(),
    dissident_html:test(),
    mochifmt:test(),
    test_request(),
    ok.

reload() ->
    [c:l(Module) || Module <- all_loaded()].

all_loaded() ->
    all_loaded(filename:dirname(code:which(?MODULE))).

all_loaded(Base) when is_atom(Base) ->
    [];
all_loaded(Base) ->
    FullBase = Base ++ "/",
    F = fun ({_Module, Loaded}, Acc) when is_atom(Loaded) ->
                Acc;
            ({Module, Loaded}, Acc) ->
                case lists:prefix(FullBase, Loaded) of
                    true ->
                        [Module | Acc];
                    false ->
                        Acc
                end
        end,
    lists:foldl(F, [], code:all_loaded()).


%% @spec new_request({Socket, Request, Headers}) -> dissidentRequest
%% @doc Return a dissident_request data structure.
new_request({Socket, {Method, {abs_path, Uri}, Version}, Headers}) ->
    dissident_request:new(Socket,
                         Method,
                         Uri,
                         Version,
                         dissident_headers:make(Headers));
% this case probably doesn't "exist".
new_request({Socket, {Method, {absoluteURI, _Protocol, _Host, _Port, Uri},
                      Version}, Headers}) ->
    dissident_request:new(Socket,
                         Method,
                         Uri,
                         Version,
                         dissident_headers:make(Headers));
%% Request-URI is "*"
%% From http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.1.2
new_request({Socket, {Method, '*'=Uri, Version}, Headers}) ->
    dissident_request:new(Socket,
                         Method,
                         Uri,
                         Version,
                         dissident_headers:make(Headers)).

%% @spec new_response({Request, integer(), Headers}) -> dissidentResponse
%% @doc Return a dissident_response data structure.
new_response({Request, Code, Headers}) ->
    dissident_response:new(Request,
                          Code,
                          dissident_headers:make(Headers)).

%% Internal API

test_request() ->
    R = dissident_request:new(z, z, "/foo/bar/baz%20wibble+quux?qs=2", z, []),
    "/foo/bar/baz wibble quux" = R:get(path),
    ok.

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
