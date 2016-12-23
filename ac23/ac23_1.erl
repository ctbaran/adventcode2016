-module(ac23_1).
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
	Result = eval({[], ASM}, dict:store("a", 7, RegDict)),
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
make_instruction(["jnz",Reg,Dist]) -> {jnz, Reg, Dist};
make_instruction(["tgl",Reg]) -> {tgl, Reg}.

eval({_Prev, []}, RegDict) -> RegDict;
eval({Prev, [Current|Next]}, RegDict) ->
	case Current of 
		{inc, Reg} -> 
			eval({[Current|Prev],Next}, dict:update_counter(Reg, 1, RegDict));
		{dec, Reg} ->
			eval({[Current|Prev],Next}, dict:update_counter(Reg, -1, RegDict));
		{jnz, Cmp, Dist} ->
			CmpVal = maybe_reg(Cmp, RegDict),
			case CmpVal =:= 0 of
				true -> eval({[Current|Prev],Next}, RegDict);
				false -> 
					NewZipper = move_zipper({Prev, [Current|Next]}, maybe_reg(Dist, RegDict)),
					eval(NewZipper, RegDict)
			end;
		{cpy,Val,[Reg]} ->
			eval({[Current|Prev],Next}, dict:store([Reg], maybe_reg(Val, RegDict), RegDict));
		{cpy,_Val,_Reg} -> eval({[Current|Prev],Next}, RegDict);
		{tgl,Reg} ->
			{ok, Dist} = dict:find(Reg, RegDict),
			{NewPrev, [NewCur|NewNext]} = toggle({Prev, [Current|Next]}, Dist),
			eval({[NewCur|NewPrev], NewNext}, RegDict)
	end.

maybe_reg(Val, RegDict) ->
	case dict:find(Val, RegDict) of
		{ok, RegVal} -> RegVal;
		error -> list_to_integer(Val)
	end.

move_zipper(Zipper, 0) -> Zipper;
move_zipper({_Prev, []}, Dist) when Dist < 0 -> {_Prev, []};
move_zipper({Prev, [Current|Next]}, Dist) when Dist >= 1 ->
	move_zipper({[Current|Prev], Next}, Dist-1);
move_zipper({[Prev|RestPrev], Next}, Dist) when Dist < 0 ->
	move_zipper({RestPrev,[Prev|Next]}, Dist+1).

toggle(Zipper, Dist) ->
	case move_zipper(Zipper, Dist) of
		{_Prev, []} -> Zipper;
		{Prev, [Cur|Next]} ->
			NewCur = toggle_instruction(Cur),
			move_zipper({Prev, [NewCur|Next]}, -Dist)
	end.

toggle_instruction({tgl,Reg}) -> {inc, Reg};
toggle_instruction({dec,Reg}) -> {inc, Reg};
toggle_instruction({inc,Reg}) -> {dec, Reg};
toggle_instruction({cpy,Val,Reg}) -> {jnz,Val,Reg};
toggle_instruction({jnz,Cmp,Dist}) -> {cpy,Cmp,Dist}.