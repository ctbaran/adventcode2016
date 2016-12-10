-module(ac10_2).
-export([solve/2]).

solve(File, Bins) ->
	{ok, FH} = file:open(File, [read]),
	Lines = read(FH,[]),
	Instructions = lists:map(fun(X) -> make_instruction(string:tokens(X, " ")) end, Lines),
	Graph = digraph:new([acyclic]),
	SortedInstructions = lists:keysort(1, Instructions),
	lists:foreach( fun(X) -> add_instruction(Graph, X) end, SortedInstructions),
	lists:foldl(fun (X, Acc) ->
					[E] = digraph:edges(Graph, {output, X}),
					{E, _, _, Val} = digraph:edge(Graph, E),
					Val * Acc
				end,
				1,
				Bins).

read(File, Lines) ->
	case file:read_line(File) of
		{ok, Line} ->
			case string:right(Line, 1) of
				"\n" -> read(File, [lists:droplast(Line)|Lines]);
				_ -> read(File, [Line|Lines])
			end;
		eof -> Lines
	end.

make_instruction(["value",Value,_,_,_,BotNum]) ->
	{value, list_to_integer(Value), list_to_integer(BotNum)};
make_instruction(["bot",BotNum,_,_,_,Type1,Type1Num,_,_,_,Type2,Type2Num]) ->
	make_comparison(list_to_integer(BotNum),Type1, list_to_integer(Type1Num), Type2, list_to_integer(Type2Num)).

make_comparison(BotNum, "output", Num1, "output", Num2) -> {bot, BotNum, {low, output, Num1}, {high, output, Num2}};
make_comparison(BotNum, "output", Num1, "bot", Num2) -> {bot, BotNum, {low, output, Num1}, {high, bot, Num2}};
make_comparison(BotNum, "bot", Num1, "output", Num2) -> {bot, BotNum, {low, bot, Num1}, {high, output, Num2}};
make_comparison(BotNum, "bot", Num1, "bot", Num2) -> {bot, BotNum, {low, bot, Num1}, {high, bot, Num2}}.

add_instruction(Graph, {value, Value, BotNum}) ->
	digraph:add_vertex(Graph, {value, Value}),
	case digraph:vertex(Graph, {bot, BotNum}) of
		false -> 
			digraph:add_vertex(Graph, {bot, BotNum}),
			digraph:add_edge(Graph, {value, Value}, {bot, BotNum}, Value);
		_ -> 
			digraph:add_edge(Graph, {value, Value}, {bot, BotNum}, Value),
			add_paths(Graph, {bot, BotNum})
	end;
add_instruction(Graph, {bot, BotNum, {low, Type1, Num1}, {high, Type2, Num2}}) ->
	digraph:add_vertex(Graph, {bot, BotNum}, {{low, Type1, Num1}, {high, Type2, Num2}}),
	add_paths(Graph, {bot, BotNum}).

add_paths(_Graph, {output, _}) -> ok;
add_paths(Graph, {bot, BotNum}) ->
	{{bot, BotNum},{{low, Type1, Num1}, {high, Type2, Num2}}} = digraph:vertex(Graph, {bot, BotNum}),
	case digraph:in_edges(Graph, {bot, BotNum}) of
		[E1,E2] ->
			{E1, _, _, Val1} = digraph:edge(Graph, E1),
			{E2, _, _, Val2} = digraph:edge(Graph, E2),
			case Val1 < Val2 of
				true -> 
					digraph:del_edges(Graph, [Val1, Val2]),
					pass_chip(Graph, {bot, BotNum}, {Type1, Num1}, Val1),
					pass_chip(Graph, {bot, BotNum}, {Type2, Num2}, Val2);
				false ->
					digraph:del_edges(Graph, [Val1, Val2]),
					pass_chip(Graph, {bot, BotNum}, {Type1, Num1}, Val2),
					pass_chip(Graph, {bot, BotNum}, {Type2, Num2}, Val1)
			end;
		_ -> ok
	end.

pass_chip(Graph, {bot,BotNum}, {output, Num}, Val) ->
	digraph:add_vertex(Graph, {output, Num}),
	digraph:add_edge(Graph, {bot, BotNum}, {output, Num}, Val);
pass_chip(Graph, {bot,BotNum}, {bot, Num}, Val) ->
	digraph:add_edge(Graph, {bot, BotNum}, {bot, Num}, Val),
	add_paths(Graph, {bot, Num}).
