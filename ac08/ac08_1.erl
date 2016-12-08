-module(ac08_1).
-export([solve/1]).

-define(XBOUND, 49).
-define(YBOUND, 5).

solve(File) ->
	{ok, FH} = file:open(File, [read]),
	Lines = read(FH,[]),
	Instructions = lists:map(fun (X) -> parse_instruction(string:tokens(X, " ")) end, Lines),
	Applied = lists:foldl( fun(X, Dict) ->
					do_instruction(X, Dict)
				 end,
				 initialize_dict(),
				 lists:reverse(Instructions)),
	lists:foldl( fun({_, State}, Acc) ->
					case State of
						on -> Acc+1;
						off -> Acc
					end
				 end,
				 0,
				 dict:to_list(Applied)).	

read(File, Lines) ->
	case file:read_line(File) of
		{ok, Line} ->
			case string:right(Line, 1) of
				"\n" -> read(File, [lists:droplast(Line)|Lines]);
				_ -> read(File, [Line|Lines])
			end;
		eof -> Lines
	end.

parse_instruction([]) -> [];
parse_instruction(["rect",Coords|_]) ->
	[X, Y|_] = string:tokens(Coords, "x"),
	{rect, list_to_integer(X), list_to_integer(Y)};
parse_instruction(["rotate",_,Coord,_,Move|_]) ->
	[Axis, Val|_] = string:tokens(Coord, "="),
	Amount = list_to_integer(Move),
	case Axis of
		"x" -> {rotate, x, list_to_integer(Val), Amount};
		"y" -> {rotate, y, list_to_integer(Val), Amount}
	end.

do_instruction({rotate, Axis, Val, Amount}, Dict) ->
	case Axis of
		x -> Pixels = lists:map(fun(Y) -> {Y, dict:fetch({Val, Y}, Dict)} end, lists:seq(0, ?YBOUND)),
			 lists:foldl( fun({Y, State}, DictI) -> 
						 	Offset = (Y + Amount) rem (?YBOUND+1),
						 	dict:store({Val, Offset}, State, DictI)
						 end,
						 Dict,
						 Pixels);
		y -> Pixels = lists:map(fun(X) -> {X, dict:fetch({X, Val}, Dict)} end, lists:seq(0, ?XBOUND)),
						 lists:foldl( fun({X, State}, DictI) -> 
						 	Offset = (X + Amount) rem (?XBOUND+1),
						 	dict:store({Offset, Val}, State, DictI)
						 end,
						 Dict,
						 Pixels)
	end;
do_instruction({rect, XMax, YMax}, Dict) ->
	lists:foldl( fun(X, DictO) ->
					lists:foldl( fun (Y, DictI) ->
						dict:update({X, Y}, fun(_) -> on end, DictI)
					end,
					DictO,
					lists:seq(0, YMax-1))
				end,
				Dict,
				lists:seq(0, XMax-1)).

initialize_dict() ->
	lists:foldl( fun(X, Dict) ->
					lists:foldl( fun (Y, DictI) ->
						dict:store({X, Y}, off, DictI)
					end,
					Dict,
					lists:seq(0, ?YBOUND))
				end,
				dict:new(),
				lists:seq(0, ?XBOUND)).
					

