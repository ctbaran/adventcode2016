-module(ac19_1).
-export([solve/1]).

solve(Elves)->
    ElfQueue = lists:foldl(fun(X, Q) -> queue:in({X,1}, Q) end, queue:new(), lists:seq(1, Elves)),
    last_elf(ElfQueue).

last_elf(ElfQueue) ->
    {{value, {ElfLeft, LeftPresents}}, Q1} = queue:out(ElfQueue),
    case queue:out(Q1) of
        {{value, {_ElfRight, RightPresents}}, Q2} ->
            last_elf( queue:in({ElfLeft, LeftPresents+RightPresents},Q2) );
        {empty, Q1} -> ElfLeft
    end.