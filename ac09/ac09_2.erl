-module(ac09_2).
-export([solve/1]).

solve(File) ->
	{ok, FH} = file:open(File, [read]),
	[Lines] = read(FH,[]),
	expand_input(Lines, {0, 0}, 0, []).

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

expand_input([], {0, 0}, Length, _Buffer) -> Length;
expand_input([], {0, Repeat}, Length, Buffer) -> 
	EmptyBuffer = string:copies(lists:reverse(Buffer), Repeat),
	expand_input(EmptyBuffer, {0, 0}, Length, []);
expand_input([40|Rest], {0, 0}, Length, Buffer) ->
	UpToLeft = lists:takewhile(fun(X) -> X /= 41 end, Rest),
	ExpandIn = read_expand(UpToLeft),
	RemoveMarker = tl( lists:subtract(Rest, UpToLeft) ),
	expand_input(RemoveMarker, ExpandIn, Length, Buffer);
expand_input([_Char|Rest], {0, 0}, Length, Buffer) ->
	expand_input(Rest, {0, 0}, Length + 1, Buffer);
expand_input(String, {0, Repeat}, Length, Buffer) ->
	EmptyBuffer = string:copies(lists:reverse(Buffer), Repeat),
	expand_input(EmptyBuffer ++ String, {0, 0}, Length, []);
expand_input([Char|Rest], {X, Repeat}, Length, Buffer) ->
	expand_input(Rest, {X-1, Repeat}, Length, [Char|Buffer]).