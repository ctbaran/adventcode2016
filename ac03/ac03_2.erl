-module(ac03_2).
-export([solve/1]).

solve(File) ->
	{ok, FH} = file:open(File, [read]),
	Triangles = read(FH, []),
	valid_triangles(Triangles, 0).

read(File, Triangles) ->
	case read_three_lines(File) of
		{L1, L2, L3} ->
			[T1,T2,T3] = get_triangle(L1, L2, L3, []),
			read(File, [T1, T2, T3 | Triangles]);
		eof -> file:close(File), lists:reverse(Triangles)
	end.	

read_three_lines(File) ->
	case file:read_line(File) of
		{ok, L1} ->
			{ok, L2} = file:read_line(File),
			{ok, L3} = file:read_line(File),
			{L1, L2, L3};
		eof ->
			eof
	end.

get_triangle("\n","\n",[], Triangles) -> Triangles;
get_triangle("\n","\n","\n", Triangles) -> Triangles;
get_triangle(String1, String2, String3, Triangles) ->
	{Side1, Rest1} = string:to_integer( string:strip(String1) ),
	{Side2, Rest2} = string:to_integer( string:strip(String2) ),
	{Side3, Rest3} = string:to_integer( string:strip(String3) ),
	get_triangle(Rest1, Rest2, Rest3, [[Side1,Side2,Side3]|Triangles]).

is_valid_triangle([A, B, C]) when A + B =< C; A + C =< B; B + C =< A -> false;
is_valid_triangle(_Triangle) -> true.

valid_triangles([], Count) -> Count;
valid_triangles([Triangle|Triangles], Count) ->	
	case is_valid_triangle(Triangle) of
		true ->
			valid_triangles(Triangles, Count + 1);
		false ->
			valid_triangles(Triangles, Count)
	end.