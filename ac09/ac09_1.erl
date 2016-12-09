-module(ac09_1).
-export([solve/1]).

solve(File) ->
	{ok, FH} = file:open(File, [read]),
	[Lines] = read(FH,[]),
	length( lists:reverse( expand_input(Lines, {0, 0}, [], []) ) ).

read(File, Lines) ->
	case file:read_line(File) of
		{ok, Line} ->
			case string:right(Line, 1) of
				"\n" -> read(File, [lists:droplast(Line)|Lines]);
				_ -> read(File, [Line|Lines])
			end;
		eof -> Lines
	end.

read_expand(Instruction) ->
	[N, Repeats] = string:tokens(Instruction, "x"),
	{list_to_integer(N), list_to_integer(Repeats)}.

expand_input([], {0, 0}, Expanded, _Buffer) -> Expanded;
expand_input([], {0, Repeat}, Expanded, Buffer) -> string:copies(Buffer, Repeat) ++ Expanded;
expand_input([40|Rest], {0, 0}, Expanded, Buffer) ->
	UpToLeft = lists:takewhile(fun(X) -> X /= 41 end, Rest),
	ExpandIn = read_expand(UpToLeft),
	RemoveMarker = tl( lists:subtract(Rest, UpToLeft) ),
	expand_input(RemoveMarker, ExpandIn, Expanded, Buffer);
expand_input([Char|Rest], {0, 0}, Expanded, Buffer) ->
	expand_input(Rest, {0, 0}, [Char] ++ Expanded, Buffer);
expand_input(String, {0, Repeat}, Expanded, Buffer) ->
	expand_input(String, {0, 0}, string:copies(Buffer, Repeat) ++ Expanded, []);
expand_input([Char|Rest], {X, Repeat}, Expanded, Buffer) ->
	expand_input(Rest, {X-1, Repeat}, Expanded, [Char|Buffer]).