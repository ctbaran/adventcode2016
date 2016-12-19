-module(ac19_2r).
-export([solve/1, across/2]).

solve(Elves)->
    last_elf_all(0, 1, Elves).

last_elf_all(PreviousSolution, MaxCircleSize, MaxCircleSize) ->
    last_elf(MaxCircleSize, PreviousSolution) + 1;
last_elf_all(PreviousSolution, CurrentCircleSize, MaxCircleSize) ->
    NewSolution = last_elf(CurrentCircleSize, PreviousSolution),
    last_elf_all(NewSolution, CurrentCircleSize+1, MaxCircleSize).

last_elf(1, _) -> 0;
last_elf(CircleSize, Offset) ->
    Across = across(0, CircleSize),
    case Across =:= 1 of
        true -> NextElf = (Across+1 rem CircleSize);
        false -> NextElf = 1
    end,
    add_offset(NextElf, Offset, Across, CircleSize).

across(Pos, Size) -> (Pos + Size div 2) rem Size.

add_offset(NextElf, Offset, Across, CircleSize) ->
    OffsetElf = NextElf + Offset,
    case (Across > NextElf) and (OffsetElf >= Across) of
        true -> (OffsetElf + 1) rem CircleSize;
        false -> OffsetElf rem CircleSize
    end.
