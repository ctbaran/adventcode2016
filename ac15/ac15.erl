-module(ac15).
-export([solve/1]).

-define(OPTIONS, [{capture, all, list}]).

solve(File) ->
    {ok, FH} = file:open(File, [read]),
    Lines =  read(FH,[]),
    Discs = build_discs(Lines),
    find_safe_time(Discs, 1, 0).
 
 read(File, Lines) ->
    case file:read_line(File) of
        {ok, Line} ->
            case string:right(Line, 1) of
                "\n" -> read(File, [lists:droplast(Line)|Lines]);
                _ -> read(File, [Line|Lines])
            end;
        eof -> Lines
    end.

build_discs(Lines) ->
    lists:foldl( fun(Line, Acc) ->
                {match, [_,Disc]} = re:run(Line, "Disc #(\\d+)", ?OPTIONS),
                {match, [_,Positions]} = re:run(Line, " (\\d+) positions", ?OPTIONS),
                {match, [_,Start]} = re:run(Line, [" (\\d+)\\."], ?OPTIONS),
                [{list_to_integer(Disc), list_to_integer(Positions), list_to_integer(Start)}|Acc]
               end,
               [],
               Lines).

find_safe_time(Discs, DiscNum, N) when DiscNum > length(Discs) -> N - DiscNum;
find_safe_time(Discs, DiscNum, N) ->
    {value, {_,_,Position}, _} = lists:keytake(DiscNum, 1, Discs),
    case Position =:= 0 of
        true -> find_safe_time(advance_positions(Discs), DiscNum+1,N+1);
        false -> find_safe_time(advance_positions(Discs), 1, N+1)
    end.

advance_positions(Discs) ->
    lists:map( fun({Disc,Slots,Position}) ->
                {Disc, Slots, (Position+1) rem Slots}
               end,
               Discs).