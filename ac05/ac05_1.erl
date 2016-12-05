-module(ac05_1).
-export([solve/1]).

solve(Door) ->
	io:fwrite("~s\n", [solve_hash(Door, [], 1)]).

solve_hash(_Door, Password, _Count) when length(Password) =:= 8 -> lists:reverse(Password);
solve_hash(Door, Password, Count) ->
	Current = Door ++ integer_to_list(Count),
	case interesting( erlang:md5(Current) ) of
		{true, [Key]} -> solve_hash(Door, [Key|Password] , Count+1);
		false -> solve_hash(Door, Password, Count+1)
	end.

interesting(<<0:4, 0:4, 0:4, 0:4, 0:4, Key:4, _/binary>>) -> {true, io_lib:format("~.16B", [Key])};
interesting(_) -> false.