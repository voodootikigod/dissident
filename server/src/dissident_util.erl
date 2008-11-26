-module(dissident_util).
-export([file_path/2, file_exists/2]).

file_path(Root, Name) ->
    RelName = case hd(Name) of
        "/" -> tl(Name);
        _ -> Name
    end,
    filename:join([Root, RelName]).

file_exists(Root, Name) ->
    NamePath = file_path(Root, Name),
    case filelib:is_regular(NamePath) of 
	true ->
	    {true, NamePath};
	false ->
	    false
    end.