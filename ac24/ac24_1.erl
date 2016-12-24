-module(ac24_1).
-export([solve/1]).

solve(File) ->
    {ok, FH} = file:open(File, [read]),
    Lines = lists:reverse( read(FH,[]) ),
    Map = ets:new(map, []),
    Numbers = add_lines(0, Lines, [], Map),
    Paths = lists:foldl(fun(Number, Acc) -> 
                        [{StartingPosition, Number}] = ets:match_object(Map, {'_', Number}),
                        Visited = dict:store(StartingPosition, visited, dict:new()),
                        X = distances_to({StartingPosition, 0}, queue:new(), dict:new(), Visited, Numbers--[Number], Map),
                        dict:store(Number, X, Acc)
                      end,
                      dict:new(),
                      Numbers),
    lists:min( paths_from($0, Paths, [$0], 0) ).

read(File, Lines) ->
    case file:read_line(File) of
        {ok, Line} ->
            case string:right(Line, 1) of
                "\n" -> read(File, [lists:droplast(Line)|Lines]);
                _ -> read(File, [Line|Lines])
            end;
        eof -> Lines
    end.

add_lines(_Height, [], Numbers, _Map) -> Numbers;
add_lines(Height, [Line|Lines], Numbers, Map) ->
    NewNumbers = add_rooms(Line, 0, Height, [], Map),
    add_lines(Height+1, Lines, Numbers ++ NewNumbers, Map).

add_rooms([], _Width, _Height, Numbers, _Map) -> Numbers;
add_rooms([$#|Rest], Width, Height, Numbers, Map) ->
    ets:insert(Map, {{Width,Height}, closed}),
    add_rooms(Rest, Width+1, Height, Numbers, Map);
add_rooms([$.|Rest],Width, Height, Numbers, Map) ->
    ets:insert(Map, {{Width,Height}, open}),
    add_rooms(Rest, Width+1, Height, Numbers, Map);
add_rooms([Number|Rest], Width, Height, Numbers, Map) ->
    ets:insert(Map, {{Width,Height}, Number}),
    add_rooms(Rest, Width+1, Height, [Number|Numbers], Map).

% distances_to(_Position, _Moves, Paths, _Visited, [], _Map) -> Paths;
distances_to({{X,Y},Dist}, Moves, Paths, Visited, Numbers, Map) ->
    Possible = lists:filter(fun(Move) -> possible(Move, Map, Visited) end, [{X+1,Y},{X-1,Y},{X,Y+1},{X,Y-1}]),
    {NextVisited, NewMoves} = lists:foldl(fun(Move, {D,Q}) -> 
                                            {dict:store(Move, visited, D),queue:in({Move,Dist+1}, Q)}
                                          end, 
                                          {Visited, Moves}, Possible),
    case ets:lookup(Map, {X,Y}) of
        [{{X,Y}, open}] ->
            {{value, NextMove}, NextMoves} = queue:out(NewMoves),
            distances_to(NextMove, NextMoves, Paths, NextVisited, Numbers, Map);
        [{{X,Y}, Number}] -> 
            NextPaths = dict:store(Number, Dist, Paths),
            case Numbers -- [Number] of
                [] -> 
                    NextPaths;
                NextNumbers ->
                    {{value, NextMove}, NextMoves} = queue:out(NewMoves),
                    distances_to(NextMove, NextMoves, NextPaths, NextVisited, NextNumbers, Map)
            end
    end.

possible(Move, Map, Visited) ->
    ((ets:lookup(Map, Move) /= []) and (ets:lookup(Map,Move) /= [{Move, closed}])) and not dict:is_key(Move, Visited).

paths_from(Number, Paths, Visited, Dist) ->
    case dict:size(Paths) of
        1 -> [Dist];
        _ ->
            Edges = dict:fetch(Number, Paths),
            RemoveSelf = dict:erase(Number, Paths),
            RemoveVisited = lists:foldl(fun(X,Acc) -> dict:erase(X, Acc) end, Edges, Visited),
            dict:fold(fun(Edge, EdgeDist, Acc) ->
                        Acc ++ paths_from(Edge, RemoveSelf, [Edge|Visited], Dist+EdgeDist)
                      end,
                      [],
                      RemoveVisited)
    end.
