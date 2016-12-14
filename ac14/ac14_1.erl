-module(ac14_1).
-export([solve/1]).

-define(OPTIONS, [{capture, all, list}]).

solve(Salt) ->
	generate_up_to(Salt, 0, 64,[], []).

generate_up_to(_Salt, _Index, N, Keys, _Candidates) when length(Keys) >= N -> lists:nth(N, lists:reverse(Keys));
generate_up_to(Salt, Index, N, Keys, Candidates) ->
	Hash = generate_hash(Salt, Index),
	{NewCandidates, NewKeys} = check_candidates(Hash, Index, Keys, Candidates),
	case has_triplet(Hash) of
		{true, MP} -> 
			generate_up_to(Salt, Index+1, N, NewKeys, NewCandidates ++ [{Index, MP}]);
		false -> 
			generate_up_to(Salt, Index+1, N, NewKeys, NewCandidates)
	end.

check_candidates(Hash, Index, Keys, Candidates) ->
	FilteredCandidates = lists:filter( fun({I, _}) -> Index - I =< 1000 end, Candidates),
	AddedKeys = lists:foldl( fun({I, RE}, Acc) ->
					case has_pentuplet(RE, Hash) of
						true -> 
							[I|Acc];
						false ->
							Acc
					end
				 end,
				 Keys,
				 FilteredCandidates),
	{FilteredCandidates, AddedKeys}.

has_triplet(String) ->
	case re:run(String, "(.)\\1\\1", ?OPTIONS) of
		{match,[_,Char]} ->
			{ok, MP} = re:compile(Char++Char++Char++Char++Char),
			{true, MP};
		nomatch -> false
	end.

has_pentuplet(MP, Hash) ->
	case re:run(Hash, MP, ?OPTIONS) of
		{match,_} -> true;
		nomatch -> false
	end.

generate_hash(Salt, Index) ->
	Bin = erlang:md5(Salt ++ integer_to_list(Index)),
	binary_to_hex(Bin,[]).

binary_to_hex(<<>>, String) -> lists:reverse(String);
binary_to_hex(<<A:4, B:4, Rest/binary>>, String) -> binary_to_hex(Rest, [hex(B),hex(A)|String]).

hex(N) when N < 10 ->
	$0 + N;
hex(N) when N >= 10 ->
	$a + (N-10).