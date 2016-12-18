-module(ac18_1).
-export([solve/2]).

solve(Start,Count) ->
    generate_rows(Start, 1, 0, Count).
  
generate_rows(Current, Rows, Safe, Max) when Rows =:= Max -> count_safe(Current, Safe);
generate_rows(Current, Rows, Safe, Max) ->
    Next = generate_row(Current, []),
    generate_rows(Next, Rows+1, count_safe(Current, Safe), Max).

generate_row([A,B|Rest], []) -> generate_row([A,B|Rest], [trap(undef, A, B)]);
generate_row([A,B,C|Rest], Row) -> generate_row([B,C|Rest], [trap(A,B,C)|Row]);
generate_row([A,B], Row) -> lists:reverse([trap(A,B,undef)|Row]).

trap($^, $^, Right) when Right /= $^ -> $^;
trap(Left, $^, $^) when Left /= $^ -> $^;
trap(Left,Center,$^) when Left /= $^, Center /= $^ -> $^;
trap($^, Center, Right) when Right /= $^, Center /= $^ -> $^;
trap(_,_,_) -> $..

count_safe([], Count) -> Count;
count_safe([$.|Rest], Count) -> count_safe(Rest, Count+1);
count_safe([_|Rest], Count) -> count_safe(Rest, Count).