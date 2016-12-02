-module(ac01_2).
-export([travel/1]).

travel(Input) ->
    Tokens = string:tokens(Input, ", "),
    {X, Y} = travel_aux(Tokens, north, {0, 0}, sets:new()),
    abs(X) + abs(Y).

travel_aux([], _Direction, Coords, _Visited) -> Coords;
travel_aux([Current|Rest], Direction, {X, Y}, Visited) ->
    case string:left(Current, 1) of
        "R" -> NewDirection = change_dir(Direction, right);
        "L" -> NewDirection = change_dir(Direction, left)
    end,
    {Distance, _Rest} = string:to_integer( tl(Current) ),
    case update_Visited(Visited, {X, Y}, Distance, NewDirection) of
        {member, NewCoord} -> NewCoord;
        {not_member, NewVisited, NewCoord} -> travel_aux(Rest, NewDirection, NewCoord, NewVisited)
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

update_Visited(Visited, NewCoord, 0, _Direction) -> {not_member, Visited, NewCoord};
update_Visited(Visited, {X, Y}, Distance, Direction) -> 
    case Direction of
        north -> NewCoord = {X, Y+1};
        east -> NewCoord = {X+1,Y};
        south -> NewCoord = {X, Y-1};
        west -> NewCoord = {X-1, Y}
    end,
    case sets:is_element(NewCoord, Visited) of
        true -> {member, NewCoord};
        false -> update_Visited(sets:add_element(NewCoord, Visited), NewCoord, Distance-1, Direction)
    end.
