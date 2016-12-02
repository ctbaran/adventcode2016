-module(ac01_1).
-export([travel/1]).

travel(Input) ->
    Tokens = string:tokens(Input, ", "),
    {X, Y} = travel_aux(Tokens, north, {0, 0}),
    abs(X) + abs(Y).

travel_aux([], _Direction, Coords) -> Coords;
travel_aux([Current|Rest], Direction, {X, Y}) ->
    case string:left(Current, 1) of
        "R" -> NewDirection = change_dir(Direction, right);
        "L" -> NewDirection = change_dir(Direction, left)
    end,
    {Distance, _Rest} = string:to_integer( tl(Current) ),
    case NewDirection of
        north -> travel_aux(Rest, NewDirection, {X, Y+Distance});
        east -> travel_aux(Rest, NewDirection, {X+Distance,Y});
        south -> travel_aux(Rest, NewDirection, {X, Y-Distance});
        west -> travel_aux(Rest, NewDirection, {X-Distance, Y})
    end.

change_dir(Current, left) ->
    case Current of
        north -> west;
        west -> south;
        south -> east;
        east -> north
    end;
change_dir(Current, right) ->
    case Current of
        north -> east;
        east -> south;
        south -> west;
        west -> north
    end.