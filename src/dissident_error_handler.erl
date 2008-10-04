%% @author Chris Williams <chris@iterativedesigns.com>
%% @copyright 2008 Iterative Designs

%% @doc A more evil error handler.

-module(dissident_error_handler).
-author('Chris Williams <chris@iterativedesigns.com>').

-export([render_error/3]).

render_error(404, Req, _Reason) ->
    Req:add_response_header("Content-Type", "text/html"), 
		<<"<html>
		<head>
			<title>404 Not Found</title>
			<style type='text/css'>
				body{ background:black; color:white; font-family:helvetica}
				a, a:link, a:visited	{ color:orange; }
				div { position:absolute; bottom:0px; right:0px; width:600px; padding:33px;}
				address {font-size:10px}
			</style>
		</head>
		<body>
			<div>
				<h1>Not Found</h1>
				<p>There is only darkness down this path, <a href='javascript:history.go(-1)'>turn back now</a>.</p>
				<hr/>
				<address>Brought to by Dissident</address>
			</div>
		</body>
	</html>">>;

render_error(500, Req, Reason) ->
    Req:add_response_header("Content-Type", "text/html"),
    error_logger:error_msg("webmachine error: path=~p~n~p~n", [Req:path(), Reason]),
    STString = io_lib:format("~p", [Reason]),
    ErrorStart = "<html><head><title>500 Internal Server Error</title></head><body><h1>Internal Server Error</h1>The server encountered an error while processing this request:<br><pre>",
    ErrorEnd = "</pre><P><HR><ADDRESS>Brought to by Dissident</ADDRESS></body></html>",
    ErrorIOList = [ErrorStart,STString,ErrorEnd],
    erlang:iolist_to_binary(ErrorIOList).

