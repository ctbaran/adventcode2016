-module(ac06_2).
-export([solve/1]).

solve(File) ->
	{ok, FH} = file:open(File, [read]),
	Lines = read(FH, []),
	Processed = process_lines(Lines, dict:new()),
	lists:foldl(fun ({_Position, Counts}, String) ->
					Chars = dict:to_list(Counts),
					{Char, _} = hd(lists:keysort(2, Chars)),
					String ++ [Char]
				end,
				[],
				lists:sort(dict:to_list(Processed))).

read(File, Lines) ->
	case file:read_line(File) of
		{ok, Line} ->
			case string:right(Line, 1) of
				"\n" -> read(File, [lists:droplast(Line)|Lines]);
				_ -> read(File, [Line|Lines])
			end;
		eof -> Lines
	end.

process_lines([], Occurrences) -> Occurrences;
process_lines([Line|Lines], Occurrences) ->
	process_lines(Lines, process_line(Line, Occurrences, 0)).

process_line([], Occurrences, _Position) -> Occurrences;
process_line([Char|Chars], Occurrences, Position) ->
	case dict:find(Position, Occurrences) of
		{ok, SubDict} -> 
			NewSubDict = dict:update_counter(Char, 1, SubDict),
			process_line(Chars, dict:store(Position, NewSubDict, Occurrences), Position+1);
		error ->
			SubDict = dict:update_counter(Char, 1, dict:new()),
			process_line(Chars, dict:store(Position, SubDict, Occurrences), Position+1)
	end.
