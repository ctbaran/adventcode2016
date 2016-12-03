-module(ac03_1).
-export([solve/1]).

solve(File) ->
	{ok, FH} = file:open(File, [read]),
	Triangles = read(FH, []),
	valid_triangles(Triangles, 0).

read(File, Triangles) ->
	case file:read_line(File) of
		{ok, Line} ->
			Triangle = get_triangle(Line, []),
			read(File, [Triangle|Triangles]);
		eof -> file:close(File), lists:reverse(Triangles)
	end.	

get_triangle([], Sides) -> Sides;
get_triangle("\n", Sides) -> Sides;
get_triangle(String, Sides) ->
	{Side, Rest} = string:to_integer( string:strip(String) ),
	get_triangle(Rest, [Side|Sides]).

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