-module(ac04_2).
-export([solve/1, shift_char/2]).

solve(File) ->
	{ok, FH} = file:open(File, [read]),
	Rooms = read(FH, []),
	ValidRooms = valid_rooms(Rooms, dict:new()),
	Decrypted = decrypt_rooms(dict:to_list(ValidRooms), []),
	file:write_file("output.txt", io_lib:format("~p.~n", [Decrypted])).

decrypt_rooms([], Decrypted) -> Decrypted;
decrypt_rooms([{Serial, Tokens}|Rooms], Decrypted) ->
	decrypt_rooms(Rooms, [decrypt_room(Tokens, Serial, [])|Decrypted]).

decrypt_room([], Serial, Decrypted) -> {Serial, string:join(lists:reverse(Decrypted), " ")};
decrypt_room([Word|Rest], Serial, Decrypted) ->
	DecryptedWord = decrypt_word(Word, Serial, []),
	decrypt_room(Rest, Serial, [DecryptedWord|Decrypted]).

decrypt_word([], _Serial, Decrypted) -> lists:reverse(Decrypted);
decrypt_word([Char|Chars], Serial, Decrypted) ->
	decrypt_word(Chars, Serial, [shift_char(Char, Serial)|Decrypted]).

shift_char(Char, Serial) ->
	(((Char - 97) + Serial) rem 26) + 97.

read(File, Rooms) ->
	case file:read_line(File) of
		{ok, Line} ->
			Tokens = string:tokens(Line, "-"),
			Encrypted = lists:droplast(Tokens),
			{Serial, Rest} = string:to_integer(lists:last(Tokens)),
			case string:right(Rest, 1) of
				"\n" -> Checksum = string:sub_string(Rest, 2, string:len(Rest) - 2);
				_ -> Checksum = string:sub_string(Rest, 2, string:len(Rest) - 1)
			end,
			read(File, [{Encrypted, Serial, Checksum}|Rooms]);
		eof -> Rooms
	end.

valid_rooms([], Valid) -> Valid;
valid_rooms([{Encrypted, Serial, Checksum}|Rooms], Valid) ->
	Occurrences = get_occurrences(Encrypted, dict:new()),
	case valid_room(Occurrences, Checksum) of
		true -> valid_rooms(Rooms, dict:store(Serial, Encrypted, Valid));
		false -> valid_rooms(Rooms, Valid)
	end.

get_occurrences([], Occurrences) -> Occurrences;
get_occurrences([Token|Rest], Occurrences) ->
	NewOccurrences = process_token(Token, Occurrences),
	get_occurrences(Rest, NewOccurrences).

process_token([], Occurrences) -> Occurrences;
process_token([Char|Chars], Occurrences) -> 
	case dict:is_key(Char, Occurrences) of
		true -> process_token(Chars, dict:update_counter(Char, 1, Occurrences));
		false -> process_token(Chars, dict:store(Char, 1, Occurrences))
	end.

valid_room(Occurrences, Checksum) -> 
	CountsDir = lists:foldl(fun ({Char, Count}, Counts) ->
			dict:append(Count, Char, Counts)
		end,
		dict:new(),
		dict:to_list(Occurrences)),
	ByCounts = lists:reverse( lists:keysort(1, dict:to_list(CountsDir)) ),
	case make_checksum([], ByCounts) =:= Checksum of
		true -> true;
		_ -> false
	end.

make_checksum(SoFar, []) -> SoFar;
make_checksum(SoFar, [{_Count, Chars}|Rest]) ->
	case string:len(SoFar) of
		5 -> SoFar;
		_ -> make_checksum(process_entry(SoFar, lists:sort(Chars)), Rest)
	end.
	
process_entry([], [Entry|Entries]) -> process_entry([Entry], Entries);
process_entry(SoFar, []) -> SoFar;
process_entry(SoFar, [Entry|Entries]) ->
	case string:len(SoFar) of
		5 -> SoFar;
		_ -> process_entry(SoFar ++ [Entry], Entries)
	end.