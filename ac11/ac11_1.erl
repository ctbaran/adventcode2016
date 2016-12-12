-module(ac11_1).
-export([solve/1, all_subsets/2]).

-define(OPTIONS, [{capture, all, list}]).

solve(File) ->
	{ok, FH} = file:open(File, [read]),
	Lines = lists:reverse( read(FH,[]) ),
	InitialState = build_floors(Lines, [], present, 1),
	% strip out all microchips + generators to determine final state
	{AllMicro,AllGen} = lists:foldl( fun ({_,_,M,G},{MS,GS})->
										{ordsets:union(M,MS), ordsets:union(G, GS)}
									end,
									{ordsets:new(),ordsets:new()},
									InitialState),
	extend_paths(ets:new(states, []), {InitialState, 0}, [{1,absent,[],[]},{2,absent,[],[]},{3,absent,[],[]},{4,present,AllMicro,AllGen}], queue:new()).

read(File, Lines) ->
	case file:read_line(File) of
		{ok, Line} ->
			case string:right(Line, 1) of
				"\n" -> read(File, [lists:droplast(Line)|Lines]);
				_ -> read(File, [Line|Lines])
			end;
		eof -> Lines
	end.

% build state from parsed instructions
build_floors([],Floors,_State,_N) -> Floors;
build_floors([Line|Lines],Floors,State,N) ->
	Microchips = ordsets:from_list( get_all_matches(Line, "(\\S)\\S+-\\S+ microchip", []) ),
	Generators = ordsets:from_list( get_all_matches(Line, "(\\S)\\S+ generator", []) ),
	build_floors(Lines, Floors ++ [{N, State, Microchips, Generators}],absent,N+1). 

% recursively match all occurrences of a pattern (work-around with the way tupling works with global)
get_all_matches([], _RE, Matches) -> Matches;
get_all_matches(Line, RE, Matches) ->
	case re:run(Line, RE, ?OPTIONS) of
		{match, [Match, Capture]} -> get_all_matches(Line -- Match, RE, [Capture|Matches]);
		nomatch -> Matches
	end.

% tail recursive BF of state space
extend_paths(_States, {TargetState, Distance}, TargetState, _ToVisit) -> Distance;
extend_paths(States, {State, Distance}, TargetState, ToVisit) ->
	StateVisited = ets:insert(States, {State, visited}),
	Transitions = get_transitions(State),
	UpdatedToVisit = lists:foldl( fun(Transition, Q) ->
						case ets:match(States, Transition) of
							[_] ->
								Q;
							[] -> 
								{ets:insert(States, {Transition, unvisited}),queue:in({Transition,Distance+1}, Q)}
						end
				   end,
				   ToVisit,
				   Transitions),
	{{value, NextState}, NextToVisit} = queue:out(UpdatedToVisit),
	extend_paths(States, NextState, TargetState, NextToVisit).

is_safe({_Floor, _, _Microchips, []}) -> true;
is_safe({_Floor, _, Microchips, Generators}) -> ordsets:size( ordsets:subtract(Microchips, Generators) ) == 0.

% gets all possible valid transitions from one state to another
get_transitions(State) ->
	{value,{CF,present,CM,CG},OtherFloors} = lists:keytake(present, 2, State),
	LowerFloor = get_floor(CF-1, OtherFloors),
	UpperFloor = get_floor(CF+1, OtherFloors),
	lists:foldl( fun (X, Acc) ->
					Acc ++ swap_floors({CF,present,CM,CG}, X, State)
				 end,
				[],
				UpperFloor ++ LowerFloor).

get_floor(N,Floors) ->
	case lists:keytake(N, 1, Floors) of
		{value,Floor,_Floors} -> [Floor];
		_ -> []
	end.

% generates all possible 
swap_floors({CF,present,CM,CG},{TF,absent,TM,TG},State) ->
	lists:foldl(fun({M,G}, Acc) ->
					NextFloor = {TF,present,ordsets:union(M,TM),ordsets:union(G,TG)},
					OldFloor= {CF,absent,ordsets:subtract(CM,M),ordsets:subtract(CG,G)},
					case is_safe(NextFloor) and is_safe(OldFloor) of
						true -> S1 = lists:keyreplace(TF,1,State,NextFloor),
								S2 = lists:keyreplace(CF,1,S1,OldFloor),
								[S2|Acc];
						false -> Acc
					end
				end,
				[],
				all_subsets({CM,CG}, 2)). 

% plain old recursive generator for all subsets
all_subsets([], Subsets, Prefix, _N) -> [Subsets ++ Prefix];
all_subsets(_, Subsets, Prefix, 0) -> [Subsets ++ Prefix];
all_subsets([A|Rest], Subsets, Prefix, N) ->
	Subsets ++ all_subsets(Rest, Subsets, Prefix++[A],N-1) ++ all_subsets(Rest,Subsets,Prefix,N). 

% gets all subsets between a pair of tuples of a given combined length
all_subsets({A,B},N) ->
	lists:foldl( fun({AN,BN}, Acc) ->
					Acc ++ [{M,G} || M <- all_subsets(A, [], [], AN), G <- all_subsets(B, [], [], BN), (M /= []) or (G /= []) ]
				 end,
				 [],
				 [ {N-K,K} || K <- lists:seq(0,N)]).
