-module(dissident_util).
-export(file_path/2, file_exists/2).

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