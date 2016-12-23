-module(ac22_2).
-export([solve/1]).

-define(XBOUND, 31).
-define(YBOUND, 27).
-define(OPTIONS, [{capture, all, list}]).

solve(File) ->
    {ok, FH} = file:open(File, [read]),
    Lines = lists:filter(fun(X) -> string:str(X, "node") /= 0 end, lists:reverse( read(FH,[]) )),
    Nodes = dict:from_list( lists:map(fun(X) -> read_node(string:tokens(X, " ")) end, Lines) ),
    move_goal({?XBOUND, 0}, Nodes).

read(File, Lines) ->
    case file:read_line(File) of
        {ok, Line} ->
            case string:right(Line, 1) of
                "\n" -> read(File, [lists:droplast(Line)|Lines]);
                _ -> read(File, [Line|Lines])
            end;
        eof -> Lines
    end.

read_node([Node, _Size, Used, Avail, _Percent]) ->
    {match, [_,X,Y]} = re:run(Node, "x(\\d+)-y(\\d+)", ?OPTIONS),
    {{list_to_integer(X),list_to_integer(Y)},{read_int(Used), read_int(Avail)}}.

read_int(X) -> {XInt, _} = string:to_integer(X), XInt.

% move_empty({1,0}, Distance, _MoveQueue, Nodes, _Visited) -> {Distance, Nodes};
move_empty({30,0}, Distance, _MoveQueue, Nodes, _Visited) -> {Distance, Nodes};
move_empty({X,Y}, Distance, MoveQueue, Nodes, Visited) ->
    case ets:lookup(Visited, {X,Y}) of
        [{{X,Y}, visited}] -> 
            {{value, {NextNode, NextDistance, NextNodes}}, NextQueue} = queue:out(MoveQueue),
            move_empty(NextNode, NextDistance, NextQueue, NextNodes, Visited);
        _ ->
            ets:insert(Visited, {{X,Y},visited}),
            GoodMoves = lists:filter(fun(Node) -> possible(Node) end, [{X+1,Y},{X,Y-1},{X-1,Y},{X,Y+1}]),
            ViableMoves = lists:filter(fun(Node2) -> viable(Node2, {X,Y}, Nodes) end, GoodMoves),
            NewQueue = lists:foldl(fun(Node,Q) -> 
                                        case ets:lookup(Visited, Node) of
                                            [{Node, unvisited}] ->
                                                io:fwrite("~p\n", [Node]),
                                                queue:in({Node, Distance+1, move_data(Node, {X,Y}, Nodes)},Q);
                                            _ -> Q
                                        end
                                   end, 
                                   MoveQueue, ViableMoves),
            {{value, {NextNode, NextDistance, NextNodes}}, NextQueue} = queue:out(NewQueue),
            move_empty(NextNode, NextDistance, NextQueue, NextNodes, Visited)
    end.

initialize_empty(Table) ->
    lists:foreach(fun(Node) -> ets:insert(Table, {Node,unvisited}) end, 
                  [{X,Y} || X<-lists:seq(0,?XBOUND),Y<-lists:seq(0,?YBOUND)]).

move_goal({X,Y}, Nodes) ->
    Visited = ets:new(visited, []),
    initialize_empty(Visited),
    {Distance, Empty} = move_empty({24,22},0,queue:new(), Nodes, Visited),
    move_goal_aux({X,Y}, Distance, Empty).

move_goal_aux({0,0}, N, _Nodes) -> N;
move_goal_aux({X,Y}, N, Nodes) ->
    Next = {X-1, Y},
    case viable({X,Y}, Next, Nodes) of
        true -> 
            move_goal_aux(Next, N+1, move_data({X,Y}, Next, Nodes));
        false ->
            {MovesToFree, FreedNext} = free_next(Next, [{X,Y}], 0, Nodes),
            move_goal_aux(Next, N+MovesToFree+1, move_data({X,Y}, Next, FreedNext))
    end.

free_next({X,Y}, Visited, Distance, Nodes) ->
    PossibleNexts = lists:filter(fun(Node) -> possible(Node) end, [{X+1,Y},{X,Y+1},{X,Y-1},{X-1,Y}] -- Visited),
    ViableNexts = lists:filter(fun(Node2) -> viable({X,Y}, Node2, Nodes) end, PossibleNexts),
    case length(ViableNexts) > 0 of
        true ->
            Node = hd(ViableNexts),
            {Distance+1, move_data({X,Y}, Node, Nodes)};
        false ->
            AdjustedNexts = lists:map(fun(Node) -> {Distance, [{X,Y}|Visited], Node} end, PossibleNexts),
            search_next(queue:from_list(AdjustedNexts), Nodes)
    end.

search_next(Nexts, Nodes) ->
    {{value, {Distance, Visited, {X,Y}}}, NextsRemoved} = queue:out(Nexts),
    PossibleNexts = lists:filter(fun(Node) -> possible(Node) end, [{X+1,Y},{X,Y+1},{X,Y-1},{X-1,Y}] -- Visited),
    ViableNexts = lists:filter(fun(Node2) -> viable({X,Y}, Node2, Nodes) end, PossibleNexts),
    case length(ViableNexts) > 0 of
        true ->
            free_next({X,Y}, Visited, Distance+1, Nodes);
        false ->
            AdjustedNexts = lists:map(fun(Node) -> {Distance+1, [{X,Y}|Visited], Node} end, PossibleNexts),
            NextsAdded = lists:foldl(fun(Elem, Q) -> queue:in(Elem, Q) end, NextsRemoved, AdjustedNexts),
            search_next(NextsAdded, Nodes)
    end.

possible({X,Y}) when X >= 0, X =< ?XBOUND, Y >= 0, Y =< ?YBOUND -> true;
possible({_X,_Y}) -> false.

viable(Node1, Node2, Nodes) ->
    {ok, {Usage1,_Avail1}} = dict:find(Node1, Nodes),
    {ok, {_Usage2,Avail2}} = dict:find(Node2, Nodes),
    Avail2 >= Usage1.

move_data(Node1, Node2, NodeDict) ->
    {Usage1, Avail1} = dict:fetch(Node1, NodeDict),
    {Usage2, Avail2} = dict:fetch(Node2, NodeDict),
    Stored1 = dict:store(Node1, {0, Avail1+Usage1}, NodeDict),
    dict:store(Node2, {Usage2+Usage1, Avail2-Usage1}, Stored1).
