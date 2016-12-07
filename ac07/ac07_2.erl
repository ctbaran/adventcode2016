-module(ac07_2).
-export([solve/1]).

solve(File) ->
	{ok, FH} = file:open(File, [read]),
	Lines = read(FH, []),
	Addresses = lists:map( fun(X) -> extract_hypernet(X, false, [], [], []) end, Lines),
	length( lists:filter( fun(X) -> supports_ssl(X) end, Addresses) ).

read(File, Lines) ->
	case file:read_line(File) of
		{ok, Line} ->
			case string:right(Line, 1) of
				"\n" -> read(File, [lists:droplast(Line)|Lines]);
				_ -> read(File, [Line|Lines])
			end;
		eof -> Lines
	end.

extract_hypernet([], _Hypernet, String, Regular, Hyper) -> 
	{[String|Regular], Hyper};
extract_hypernet([91|Rest], false, String, Regular, Hyper) ->
	extract_hypernet(Rest, true, [], [String|Regular], Hyper);
extract_hypernet([93|Rest], true, String, Regular, Hyper) -> 
	extract_hypernet(Rest, false, [], Regular, [String|Hyper]);
extract_hypernet([Char|Rest], false, String, Regular, Hyper) -> 
	extract_hypernet(Rest, false, [Char|String], Regular, Hyper);
extract_hypernet([Char|Rest], true, String, Regular, Hyper) -> 
	extract_hypernet(Rest, true, [Char|String], Regular, Hyper).

supports_ssl({Regular, Hypernet}) ->
	ABAs = lists:flatmap( fun(X) -> get_abas(X, []) end, Regular),
	lists:foldl( fun (X, Acc) -> has_matching_bab(X, ABAs) or Acc end, false, Hypernet).

get_abas([], ABAs) -> ABAs;
get_abas([A,B,A|Rest], ABAs) when A /= B -> get_abas([B,A|Rest], [[A,B,A]|ABAs]);
get_abas([_|Rest], ABAs) -> get_abas(Rest, ABAs).

matching_bab([], _ABA) -> false;
matching_bab([B,A,B|_], [A,B,A]) -> true;
matching_bab([_|Rest], ABA) -> matching_bab(Rest, ABA).

has_matching_bab(Hypernet, ABAs) ->
	lists:foldl( fun(X,Acc) -> matching_bab(Hypernet, X) or Acc end, false, ABAs).