-module(ac16_1).
-export([solve/2]).


solve(Input, Size) ->
	Sequence = rewrite(Input, Size),
	checksum( reduce_checksum(Sequence, []) ).

rewrite(A, Size) when length(A) >= Size -> {N1, _} = lists:split(Size, A), N1;
rewrite(A, Size) ->
	B = invert(A, []),
	rewrite(A ++ [$0] ++ B, Size).

invert([], B) -> B;
invert([$0|Rest], B) -> invert(Rest, [$1|B]);
invert([$1|Rest], B) -> invert(Rest, [$0|B]).

checksum(Checksum) when length(Checksum) rem 2 =:= 1 -> Checksum;
checksum(Checksum) -> checksum( reduce_checksum(Checksum, []) ).

reduce_checksum([], Checksum) ->  lists:reverse(Checksum);
reduce_checksum([A,A|Rest], Checksum) -> reduce_checksum(Rest, [$1|Checksum]);
reduce_checksum([A,B|Rest], Checksum) when A /= B -> reduce_checksum(Rest, [$0|Checksum]).