-module(ac20_1).
-export([solve/1]).

solve(File) ->
    {ok, FH} = file:open(File, [read]),
    Lines = read(FH,[]),
    [{Start, End}|Ranges] = lists:sort( extract_ranges(Lines, []) ),
    case Start > 0 of
        true -> Start - 1;
        false -> find_lowest(Ranges, {Start, End})
    end.

 read(File, Lines) ->
    case file:read_line(File) of
        {ok, Line} ->
            case string:right(Line, 1) of
                "\n" -> read(File, [lists:droplast(Line)|Lines]);
                _ -> read(File, [Line|Lines])
            end;
        eof -> Lines
    end.

extract_ranges([], Ranges) -> Ranges;
extract_ranges([Range|Rest], Ranges) ->
    [Lower, Upper] = lists:map(fun(X) -> list_to_integer(X) end, string:tokens(Range, "-")),
    extract_ranges(Rest, [{Lower,Upper}|Ranges]).

find_lowest([], {_Start, End}) -> End + 1;
find_lowest([{NewStart, _NewEnd}|_Ranges], {_OldStart, OldEnd}) 
    when NewStart > (OldEnd + 1) -> OldEnd + 1;
find_lowest([{NewStart, NewEnd}|Ranges], {_OldStart, OldEnd}) 
    when NewStart >= OldEnd -> find_lowest(Ranges, {NewStart, NewEnd});
find_lowest([{NewStart, NewEnd}|Ranges], {OldStart, OldEnd})
    when (NewStart > OldStart) and (NewEnd > OldEnd) -> find_lowest(Ranges, {NewStart, NewEnd});
find_lowest([{_NewStart, _NewEnd}|Ranges], {OldStart, OldEnd}) ->
    find_lowest(Ranges, {OldStart, OldEnd}).
