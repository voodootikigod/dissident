%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2008 Basho Technologies
%% (guess_mime/1 derived from code copyright 2007 Mochi Media, Inc.)
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

%% @doc Utilities for parsing and quoting.

-module(webmachine_util).
-export([guess_mime/1]).
-export([convert_request_date/1, compare_ims_dates/2]).
-export([if_match_etag/1]).
-export([choose_media_type/2]).
-export([now_diff_milliseconds/2]).

convert_request_date(Date) ->
    try 
	case httpd_util:convert_request_date(Date) of
	    ReqDate -> ReqDate
	end
    catch
	error:_ -> bad_date
    end.

%% returns true if D1 > D2
compare_ims_dates(D1, D2) ->
    GD1 = calendar:datetime_to_gregorian_seconds(D1),
    GD2 = calendar:datetime_to_gregorian_seconds(D2),
    GD1 > GD2.

if_match_etag(_ETagHdr) ->
    false.

%% @spec guess_mime(string()) -> string()
%% @doc  Guess the mime type of a file by the extension of its filename.
guess_mime(File) ->
    case filename:extension(File) of
	".html" ->
	    "text/html";
	".xhtml" ->
	    "application/xhtml+xml";
	".xml" ->
	    "application/xml";
	".css" ->
	    "text/css";
	".js" ->
	    "application/x-javascript";
	".jpg" ->
	    "image/jpeg";
	".jpeg" ->
	    "image/jpeg";
	".gif" ->
	    "image/gif";
	".png" ->
	    "image/png";
	".ico" ->
	    "image/x-icon";
	".swf" ->
	    "application/x-shockwave-flash";
	".zip" ->
	    "application/zip";
	".bz2" ->
	    "application/x-bzip2";
	".gz" ->
	    "application/x-gzip";
	".tar" ->
	    "application/x-tar";
	".tgz" ->
	    "application/x-gzip";
        ".htc" ->
            "text/x-component";
	_ ->
	    "text/plain"
    end.

choose_media_type(Provided,AcceptHead) ->
    % Return the Content-Type we will serve for a request.
    % If there is no acceptable/available match, return the atom "none".
    % AcceptHead is the value of the request's Accept header
    % Provided is a list of media types the resource can provide.
    %  each is either a string e.g. -- "text/html"
    %   or a string and parameters e.g. -- {"text/html",[{level,1}]}
    % (the plain string case with no parameters is much more common)
    Requested = accept_header_to_media_types(AcceptHead),
    Prov1 = normalize_provided(Provided),
    choose_media_type1(Prov1,Requested).
choose_media_type1(_Provided,[]) ->
    none;
choose_media_type1(Provided,[H|T]) ->
    {_Pri,Type,Params} = H,
    case media_match({Type,Params}, Provided) of
	[] -> choose_media_type1(Provided,T);
	[{CT_T,CT_P}|_] -> format_content_type(CT_T,CT_P)
    end.

media_match(_,[]) -> none;
media_match({"*/*",[]},[H|_]) -> [H];
media_match({Type,Params},Provided) ->
    [{T1,P1} || {T1,P1} <- Provided,
		media_type_match(Type,T1), media_params_match(Params,P1)].
media_type_match(Req,Prov) ->
    case Req of
	"*" -> % might as well not break for lame (Gomez) clients
	    true;
	"*/*" ->
	    true;
	Prov ->
	    true;
	_ ->
	    [R1,R2] = string:tokens(Req,"/"),
	    [P1,_P2] = string:tokens(Prov,"/"),
	    case R2 of
		"*" ->
		    case R1 of
			P1 -> true;
			_ -> false
		    end;
		_ -> false
	    end
    end.
media_params_match(Req,Prov) ->
    lists:sort(Req) =:= lists:sort(Prov).	    

prioritize_media(TyParam) ->
    {Type, Params} = TyParam,
    prioritize_media(Type,Params,[]).    
prioritize_media(Type,Params,Acc) ->
    case Params of
	[] ->
	    {1, Type, Acc};
	_ ->
	    [{Tok,Val}|Rest] = Params,
	    case Tok of
		"q" ->
		    QVal = case Val of
			"1" ->
			    1;
			_ -> list_to_float(Val)
		    end,
		    {QVal, Type, Rest ++ Acc};
		_ ->
		    prioritize_media(Type,Rest,[{Tok,Val}|Acc])
	    end
    end.

accept_header_to_media_types(HeadVal) ->
    % given the value of an accept header, produce an ordered list
    % based on the q-values.  Results are [{Type,Params}] with the
    % head of the list being the highest-priority requested type.
    try
        lists:reverse(lists:keysort(1,
         [prioritize_media({Type,[list_to_tuple(string:tokens(X,"="))
          || X <- Params]}) || [Type|Params] <- [string:tokens(X, ";")
          || X <- [string:strip(X) || X <- string:tokens(HeadVal, ",")]]]))
    catch _:_ -> []
    end.

normalize_provided(Provided) ->
    [normalize_provided1(X) || X <- Provided].
normalize_provided1(Type) when is_list(Type) -> {Type, []};
normalize_provided1({Type,Params}) -> {Type, Params}.

format_content_type(Type,[]) -> Type;
format_content_type(Type,[H|T]) -> format_content_type(Type ++ "; " ++ H, T).


%% @type now() = {MegaSecs, Secs, MicroSecs}

%% This is faster than timer:now_diff() because it does not use bignums.
%% But it returns *milliseconds*  (timer:now_diff returns microseconds.)
%% From http://www.erlang.org/ml-archive/erlang-questions/200205/msg00027.html

%% @doc  Compute the difference between two now() tuples, in milliseconds.
%% @spec now_diff_milliseconds(now(), now()) -> integer()
now_diff_milliseconds({M,S,U}, {M,S1,U1}) ->
    ((S-S1) * 1000) + ((U-U1) div 1000);
now_diff_milliseconds({M,S,U}, {M1,S1,U1}) ->
    ((M-M1)*1000000+(S-S1))*1000 + ((U-U1) div 1000).
