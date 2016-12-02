-module(ac02_2).
-export([solve/1]).

-define(BOUND, 2).

solve(File) ->
	{ok, FH} = file:open(File, [read]),
	find_digits(read(FH,[]), {-2, 0}, []).

read(File, Lines) ->
	case file:read_line(File) of
		{ok, Line} -> read(File, [Line | Lines]);
		eof -> file:close(File), lists:reverse(Lines)
	end.

find_digits([], _Start, Digits) -> lists:reverse(Digits);
find_digits([Code|Rest], Position, Digits) ->
	NewPosition = find_digit(Code, Position),
	Digit = digit_from_position(NewPosition),
	find_digits(Rest, NewPosition, [Digit|Digits]).

find_digit("\n", Position) -> Position;
find_digit([], Position) -> Position;
find_digit([Movement|Rest], {X, Y}) ->
	case [Movement] of
		"U" -> NewPosition = {X, Y+1};
		"D"	-> NewPosition = {X, Y-1};
		"L"	-> NewPosition = {X-1, Y};
		"R" -> NewPosition = {X+1, Y}
	end,
	case out_of_bounds(NewPosition) of
		true -> find_digit(Rest, {X, Y});
		false -> find_digit(Rest, NewPosition)
	end.

out_of_bounds({X, Y}) when abs(X)+abs(Y) > ?BOUND -> true;
out_of_bounds(_Coords) -> false.

digit_from_position({X, Y}) ->
	XMax = (?BOUND - abs(Y)) + 1,
	XDist = (X + XMax),
	YDist = lists:foldl( fun (Row, Sum) -> Sum + ((?BOUND - abs(Row)) * 2 + 1) end,
		0,
		lists:seq(-?BOUND, -Y-1) ),
	Digit = XDist + YDist,
	get_digit(Digit).

get_digit(Digit) when Digit < 10 -> Digit + 48;
get_digit(Digit) ->
	Offset = Digit - 10,
	65 + Offset.