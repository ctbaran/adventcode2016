-module(ac22_1).
-export([solve/1]).

-define(OPTIONS, [{capture, all, list}]).

solve(File) ->
    {ok, FH} = file:open(File, [read]),
    Lines = lists:filter(fun(X) -> string:str(X, "node") /= 0 end, lists:reverse( read(FH,[]) )),
    Nodes = lists:map(fun(X) -> read_node(string:tokens(X, " ")) end, Lines),
    ByAvail = lists:reverse( lists:keysort(4, Nodes) ),
    lists:foldl(fun({X,_,Usage,_,PCent}, Acc) -> 
                    case PCent =:= 0 of
                        true -> Acc;
                        false -> Acc + length( viable({X,Usage},ByAvail,[]) ) 
                    end
                end,
                0, 
                ByAvail).

 read(File, Lines) ->
    case file:read_line(File) of
        {ok, Line} ->
            case string:right(Line, 1) of
                "\n" -> read(File, [lists:droplast(Line)|Lines]);
                _ -> read(File, [Line|Lines])
            end;
        eof -> Lines
    end.

read_node([Node, Size, Used, Avail, Percent]) ->
    {match, [_,X,Y]} = re:run(Node, "x(\\d+)-y(\\d+)", ?OPTIONS),
    {{list_to_integer(X),list_to_integer(Y)},read_int(Size), read_int(Used), read_int(Avail), read_int(Percent)}.

read_int(X) -> {XInt, _} = string:to_integer(X), XInt.

viable({_Node1, _Usage}, [], Viable) -> Viable;
viable({Node1, Usage}, [{Node1,_,_,_,_}|OtherNodes], Viable) -> viable({Node1, Usage}, OtherNodes, Viable);
viable({_Node1, Usage}, [{_Node2,_,_,Avail,_}|_], Viable) when Avail < Usage -> Viable;
viable({Node1, Usage}, [{Node2,_,_,_Avail,_}|OtherNodes], Viable) -> viable({Node1, Usage},OtherNodes,[{Node1,Node2}|Viable]).
