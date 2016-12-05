-module(ac05_2).
-export([solve/1]).

solve(Door) ->
	Positions = solve_hash(Door, dict:new(), 1, 0),
	Password = lists:foldl( fun ({_Pos, Char}, SoFar) -> SoFar ++ Char end,
		[],
		lists:keysort(1, dict:to_list(Positions))),
	io:fwrite("~s\n", [Password]).

solve_hash(_Door, Password, _Count, Found) when Found =:= 8 -> Password;
solve_hash(Door, Password, Count, Found) ->
	Current = Door ++ integer_to_list(Count),
	case interesting( erlang:md5(Current) ) of
		{true, Position,[Char]} -> 
			case dict:is_key(Position, Password) of
				true -> solve_hash(Door, Password, Count+1, Found);
				false -> 
					solve_hash(Door, dict:store(Position, Char, Password), Count+1, Found +1)
			end;
		false -> solve_hash(Door, Password, Count+1, Found)
	end.

interesting(<<0:4, 0:4, 0:4, 0:4, 0:4, Position:4, Char:4, _:4, _/binary>>) -> 
	case Position < 8 of
		true -> {true, Position, io_lib:format("~.16B", [Char])};
		false -> false
	end;
interesting(_) -> false.