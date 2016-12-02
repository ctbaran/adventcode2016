-module(ac02_1).
-export([solve/1]).

-define(BOUND, 1).

solve(File) ->
	{ok, FH} = file:open(File, [read]),
	find_digits(read(FH,[]), {0, 0}, []).

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

out_of_bounds({X, _Y}) when abs(X) > ?BOUND -> true;
out_of_bounds({_X, Y}) when abs(Y) > ?BOUND -> true;
out_of_bounds(_Coords) -> false.

digit_from_position({X, Y}) ->
	XDist = (X + ?BOUND) + 1,
	YDist = (?BOUND * 2 + 1) * abs((?BOUND-Y)),
	Digit = XDist + YDist,
	Digit + 48.
