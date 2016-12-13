-module(ac13_2).
-export([solve/2]).

solve(MaxSteps, FavNum) ->
	Maze = initialize_maze(51,51,FavNum),
	find_target(Maze, {{1,1}, 0}, MaxSteps, queue:new()),
	length( ets:match(Maze, {'_', open, visited}) ).

compute(X, Y, FavNum) ->
	Num = (X*X) + (3*X) + (2*X*Y) + Y + (Y*Y),
	Bin = integer_to_list( Num + FavNum, 2),
	case bits(Bin, 0) rem 2 == 0 of
		true -> open;
		false -> closed
	end.

bits([], Ones) -> Ones;
bits([49|Rest], Ones) -> bits(Rest, Ones+1);
bits([_|Rest], Ones) -> bits(Rest, Ones).


initialize_maze(XMax, YMax, FavNum) ->
	Maze = ets:new(maze, []),
	lists:foreach( fun({X,Y}) ->
					ets:insert(Maze, {{X,Y}, compute(X,Y, FavNum), unvisited})
				 end,
				 [ {X,Y} || X<-lists:seq(0,XMax), Y<-lists:seq(0,YMax) ]),
	Maze.

% tail recursive BF of state space
find_target(_Maze, {_Position, MaxSteps}, MaxSteps, {[],[]}) -> ok;
find_target(Maze, {_Position, MaxSteps}, MaxSteps, ToVisit) ->
	{{value, NextPosition}, NextToVisit} = queue:out(ToVisit),
	find_target(Maze, NextPosition, MaxSteps, NextToVisit);
find_target(Maze, {Position, Distance}, MaxSteps, ToVisit) ->
	io:fwrite("~p\n", [Position]),
	ets:update_element(Maze, Position, {3,visited}),
	Adjacent = get_adjacent(Position),
	UpdatedToVisit = lists:foldl( fun(NewPosition, Q) ->
						case ets:lookup(Maze, NewPosition) of
							[{NewPosition, closed, _}] ->
								Q;
							[{NewPosition, open, visited}] ->
								Q;
							[{NewPosition, open, unvisited}] ->
								queue:in({NewPosition,Distance+1}, Q)
						end
				   end,
				   ToVisit,
				   Adjacent),
	{{value, NextPosition}, NextToVisit} = queue:out(UpdatedToVisit),
	find_target(Maze, NextPosition, MaxSteps, NextToVisit).

get_adjacent({0,0}) -> [{0,1},{1,0}];
get_adjacent({X,0}) -> [{X,1},{X-1,0},{X+1,0}];
get_adjacent({0,Y}) -> [{1,Y},{0,Y-1},{0,Y+1}];
get_adjacent({X,Y}) -> [{X-1,Y},{X,Y-1},{X+1,Y},{X,Y+1}].