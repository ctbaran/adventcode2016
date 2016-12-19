-module(ac19_1).
-export([solve/1]).

solve(Elves)->
    ElfQueue = lists:foldl(fun(X, Q) -> queue:in(X, Q) end, queue:new(), lists:seq(1, Elves)),
    last_elf(ElfQueue).

last_elf(ElfQueue) ->
    {{value, ElfLeft}, Q1} = queue:out(ElfQueue),
    case queue:out(Q1) of
        {{value, _}, Q2} -> last_elf( queue:in(ElfLeft,Q2) );
        {empty, Q1} -> ElfLeft
    end.