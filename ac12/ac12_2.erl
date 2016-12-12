-module(ac12_2).
-export([solve/3]).

solve(File, Registers, Target) ->
	{ok, FH} = file:open(File, [read]),
	Lines = lists:reverse( read(FH,[]) ),
	ASM = make_assembly(Lines),
	RegDict = lists:foldl(fun (X, Acc) ->
								dict:store(X, 0, Acc)
							end,
							dict:new(),
							Registers),
	Result = eval({[], ASM}, dict:store("c", 1, RegDict)),
	dict:find(Target, Result).


read(File, Lines) ->
	case file:read_line(File) of
		{ok, Line} ->
			case string:right(Line, 1) of
				"\n" -> read(File, [lists:droplast(Line)|Lines]);
				_ -> read(File, [Line|Lines])
			end;
		eof -> Lines
	end.

make_assembly(Lines) ->
	Tokens = lists:map( fun(X) -> string:tokens(X, " ") end, Lines),
	lists:map( fun(X) -> make_instruction(X) end, Tokens).

make_instruction(["cpy",Val,Reg]) -> {cpy, Val, Reg};
make_instruction(["dec",Reg]) -> {dec, Reg};
make_instruction(["inc",Reg]) -> {inc, Reg};
make_instruction(["jnz",Reg,Dist]) -> {jnz, Reg, list_to_integer(Dist)}.

eval({_Prev, []}, RegDict) -> RegDict;
eval({Prev, [Current|Next]}, RegDict) ->
	case Current of 
		{inc,Reg} -> 
			eval({[Current|Prev],Next}, dict:update_counter(Reg, 1, RegDict));
		{dec, Reg} ->
			eval({[Current|Prev],Next}, dict:update_counter(Reg, -1, RegDict));
		{jnz, Cmp, Dist} ->
			case dict:find(Cmp, RegDict) of
				{ok, CmpVal} -> ok;
				error -> CmpVal = list_to_integer(Cmp)
			end,
			case CmpVal =:= 0 of
				true -> eval({[Current|Prev],Next}, RegDict);
				false -> 
					NewZipper = move_zipper({Prev, [Current|Next]}, Dist),
					eval(NewZipper, RegDict)
			end;
		{cpy,Val,Reg} ->
			case dict:find(Val, RegDict) of
				{ok, RegVal} ->
					eval({[Current|Prev],Next}, dict:store(Reg, RegVal, RegDict));
				_ ->
					eval({[Current|Prev],Next}, dict:store(Reg, list_to_integer(Val), RegDict))
			end
	end.

move_zipper(Zipper, 0) -> Zipper;
move_zipper({_Prev, []}, Dist) when Dist < 0 -> {_Prev, []};
move_zipper({Prev, [Current|Next]}, Dist) when Dist >= 1 ->
	move_zipper({[Current|Prev], Next}, Dist-1);
move_zipper({[Prev|RestPrev], Next}, Dist) when Dist < 0 ->
	move_zipper({RestPrev,[Prev|Next]}, Dist+1).
