-module(ac17_2).
-export([solve/1]).

-define(XBOUND, 4).
-define(YBOUND, 4).

solve(Input) ->
	search(Input, [], queue:new(), 0, {1,1}).

search(Input, Path, Paths, MaxSoFar, {?XBOUND, ?YBOUND}) -> 
    MaxPath = max(MaxSoFar, length(Path)),
    case queue:out(Paths) of 
        {{value,{NewPath, NewCoords}},NextPaths} ->
            search(Input, NewPath, NextPaths, MaxPath, NewCoords);
        {empty,_}  -> MaxPath
    end;
search(Input, Path, Paths, MaxPath, {X, Y}) ->
	Directions = open( md5(Input ++ Path) ),
    Possible = lists:filter(fun({_,Door}) -> Door =:= open end, Directions),
    Movements = lists:map(fun({Dir,_}) -> move(Dir, {X,Y}) end, Possible),
    NewPaths = lists:foldl(fun ({Dir,Coords}, Acc) ->
                                case in_bounds(Coords) of
                                    true -> queue:in({Path ++ Dir,Coords}, Acc);
                                    false -> Acc
                                end
                           end,
                           Paths,
                           Movements),
    case queue:out(NewPaths) of 
        {{value,{NewPath, NewCoords}},NextPaths} ->
            search(Input, NewPath, NextPaths, MaxPath, NewCoords);
        {empty,_} -> MaxPath
    end.

open([Up,Down,Left,Right|_]) ->
    lists:map(fun({Dir,Char}) -> {Dir, is_open(Char)} end, 
    [{up,Up},{down,Down},{left,Left},{right,Right}]).

move(up, {X,Y}) -> {"U", {X, Y-1}};
move(down, {X,Y}) -> {"D", {X, Y+1}};
move(left, {X,Y}) -> {"L", {X-1, Y}};
move(right, {X,Y}) -> {"R", {X+1, Y}}.

in_bounds({X,Y}) -> (X > 0) and (X =< ?XBOUND) and (Y > 0) and (Y =< ?YBOUND).

is_open(A) when A >= $b, A =< $f -> open;
is_open(_) -> closed.

md5(String) -> binary_to_hex(erlang:md5(String), []).

binary_to_hex(<<>>, String) -> lists:reverse(String);
binary_to_hex(<<A:4, B:4, Rest/binary>>, String) -> binary_to_hex(Rest, [hex(B),hex(A)|String]).

hex(N) when N < 10 -> $0 + N;
hex(N) when N >= 10 -> $a + (N-10).