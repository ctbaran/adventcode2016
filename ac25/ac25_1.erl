-module(ac25_1).
-export([solve/2, eval/3]).

solve(File, Registers) ->
	{ok, FH} = file:open(File, [read]),
	Lines = lists:reverse( read(FH,[]) ),
	ASM = make_assembly(Lines),
	receiver(0, undefined, undefined, 0, ASM, Registers).

receiver(100, _State, Evaluator, N, _ASM, _Registers) -> exit(Evaluator, kill), N;
receiver(_Count, _State, undefined, N, ASM, Registers) ->
	RegDict = lists:foldl(fun (X, Acc) ->
								dict:store(X, 0, Acc)
							end,
							dict:new(),
							Registers),
	Evaluator = spawn( ?MODULE, eval, [{[], ASM}, dict:store("a", N, RegDict), self()] ),
	receiver(0, 1, Evaluator, N, ASM, Registers);
receiver(Count, State, Evaluator, N, ASM, Registers) ->
	InvertState = (State + 1) rem 2,
	receive
		InvertState ->
			receiver(Count+1, InvertState, Evaluator, N, ASM, Registers);
		State ->
			exit(Evaluator, kill),
			flush_messages(),
			receiver(Count, State, undefined, N+1, ASM, Registers)
	after 5000 ->
		exit(Evaluator, kill),
		flush_messages(),
		receiver(Count, State, undefined, N+1, ASM, Registers)
	end.

flush_messages() ->
	receive
		_ -> flush_messages()
	after 0 ->
		ok
	end.

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
make_instruction(["tgl",Reg]) -> {tgl, Reg};
make_instruction(["out",Reg]) -> {out, Reg}.

eval({_Prev, []}, RegDict, _Clock) -> RegDict;
eval({Prev, [Current|Next]}, RegDict, Clock) ->
	case Current of 
		{inc, Reg} -> 
			eval({[Current|Prev],Next}, dict:update_counter(Reg, 1, RegDict), Clock);
		{dec, Reg} ->
			eval({[Current|Prev],Next}, dict:update_counter(Reg, -1, RegDict), Clock);
		{jnz, Cmp, Dist} ->
			CmpVal = maybe_reg(Cmp, RegDict),
			case CmpVal =:= 0 of
				true -> eval({[Current|Prev],Next}, RegDict, Clock);
				false -> 
					NewZipper = move_zipper({Prev, [Current|Next]}, maybe_reg(Dist, RegDict)),
					eval(NewZipper, RegDict, Clock)
			end;
		{out,Reg} ->
			{ok, Out} = dict:find(Reg, RegDict),
			Clock ! Out,
			eval({[Current|Prev],Next}, RegDict, Clock);
		{cpy,Val,[Reg]} ->
			eval({[Current|Prev],Next}, dict:store([Reg], maybe_reg(Val, RegDict), RegDict), Clock);
		{cpy,_Val,_Reg} -> eval({[Current|Prev],Next}, RegDict, Clock);
		{tgl,Reg} ->
			{ok, Dist} = dict:find(Reg, RegDict),
			{NewPrev, [NewCur|NewNext]} = toggle({Prev, [Current|Next]}, Dist),
			eval({[NewCur|NewPrev], NewNext}, RegDict, Clock)
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