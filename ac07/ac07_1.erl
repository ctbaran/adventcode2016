-module(ac07_1).
-export([solve/1]).

solve(File) ->
	{ok, FH} = file:open(File, [read]),
	Lines = read(FH, []),
	Addresses = lists:map( fun(X) -> extract_hypernet(X, false, [], [], []) end, Lines),
	length( lists:filter( fun(X) -> address_supports(X) end, Addresses) ).

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

address_supports({Regular, Hypernet}) ->
	case lists:foldl( fun (X, Acc) -> supports_tls(X) or Acc end, false, Hypernet) of
		true -> false;
		false -> lists:foldl(fun (X, Acc) -> supports_tls(X) or Acc end, false, Regular)
	end.

supports_tls([A,B,B,A|_]) when A /= B-> true;
supports_tls([_|Rest]) -> supports_tls(Rest);
supports_tls(_) -> false.
