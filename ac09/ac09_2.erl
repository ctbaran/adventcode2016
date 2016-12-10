-module(ac09_2).
-export([solve/1]).

solve(File) ->
	{ok, FH} = file:open(File, [read]),
	[Lines] = read(FH,[]),
	expand_input(list_to_binary(Lines), {0, 0}, 0, <<>>).

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
	[N, Repeats] = binary:split(Instruction, <<"x">>),
	{binary_to_integer(N), binary_to_integer(Repeats)}.

expand_input(<<>>, {0, 0}, Length, _Buffer) -> Length;
expand_input(<<>>, {0, Repeat}, Length, Buffer) -> 
	EmptyBuffer = binary:copy(Buffer, Repeat),
	expand_input(EmptyBuffer, {0, 0}, Length, <<>>);
expand_input(<< "(", Rest/binary>>, {0, 0}, Length, Buffer) ->
	[UpToLeft, AfterRight]  = binary:split(Rest, <<")">>),
	ExpandIn = read_expand(UpToLeft),
	expand_input(AfterRight, ExpandIn, Length, Buffer);
expand_input(<<_:8, Rest/binary>>, {0, 0}, Length, Buffer) ->
	expand_input(Rest, {0, 0}, Length + 1, Buffer);
expand_input(String, {0, Repeat}, Length, Buffer) ->
	EmptyBuffer = binary:copy(Buffer, Repeat),
	expand_input(<<EmptyBuffer/binary, String/binary>>, {0, 0}, Length, <<>>);
expand_input(<<Char:8, Rest/binary>>, {X, Repeat}, Length, <<>>) ->
	expand_input(Rest, {X-1, Repeat}, Length, <<Char>>);
expand_input(<<Char:8, Rest/binary>>, {X, Repeat}, Length, Buffer) ->
	expand_input(Rest, {X-1, Repeat}, Length, <<Buffer/binary, Char:8>>).