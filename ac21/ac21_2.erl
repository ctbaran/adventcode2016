-module(ac21_2).
-export([solve/2]).

solve(File, Password) ->
    {ok, FH} = file:open(File, [read]),
    Lines = read(FH,[]),
    Instructions = lists:map( fun(X) -> Tokens = string:tokens(X, " "), read_instruction(Tokens) end,
                              Lines),
    process_password(Password, Instructions).

 read(File, Lines) ->
    case file:read_line(File) of
        {ok, Line} ->
            case string:right(Line, 1) of
                "\n" -> read(File, [lists:droplast(Line)|Lines]);
                _ -> read(File, [Line|Lines])
            end;
        eof -> Lines
    end.

read_instruction(["swap","position",X,"with","position",Y]) -> 
    {swap_position, list_to_integer(X), list_to_integer(Y)};
read_instruction(["swap","letter",X,"with","letter",Y]) -> 
    {swap_letter, hd(X), hd(Y)};
read_instruction(["rotate",Direction,X,_]) ->
    case Direction of
        "left" -> {rotate_direction,right,list_to_integer(X)};
        "right" -> {rotate_direction,left,list_to_integer(X)}
    end;
read_instruction(["rotate","based","on","position","of","letter",X]) ->
    {rotate_letter, hd(X)};
read_instruction(["reverse","positions",X,"through",Y]) ->
    {reverse_positions, list_to_integer(X), list_to_integer(Y)};
read_instruction(["move","position",X,"to","position",Y]) ->
    {move_position, list_to_integer(Y), list_to_integer(X)}.

process_password(Password, Instructions) ->
    lists:foldl(fun(Instruction, Pass) ->
                    do_instruction(Pass, Instruction)
                end,
                Password,
                Instructions).

do_instruction(Password, {swap_position, X, Y}) -> swap_position(Password, X, Y);
do_instruction(Password, {swap_letter, X, Y}) -> swap_letters(Password, [], X, Y);
do_instruction(Password, {rotate_letter, X}) -> undo_rotate_letter(Password, X);
do_instruction(Password, {rotate_direction, Direction, Steps}) -> rotate_direction(Password, Direction, Steps);
do_instruction(Password, {move_position, X, Y}) -> move_position(Password, X, Y);
do_instruction(Password, {reverse_positions, X, Y}) -> reverse_positions(Password, X, Y).

reverse_positions(Password, X, Y) ->
    {Before, Next} = lists:split(X, Password),
    {ToReverse, After} = lists:split(Y+1-X, Next),
    Before ++ lists:reverse(ToReverse) ++ After.

swap_position(Password, X, Y) ->
    LetterX = lists:nth(X+1, Password),
    LetterY = lists:nth(Y+1, Password),
    swap_letters(Password, [], LetterX, LetterY).

swap_letters([], NewPassword, _X, _Y) -> lists:reverse(NewPassword);
swap_letters([X|Password], NewPassword, X, Y) -> swap_letters(Password, [Y|NewPassword], X, Y);
swap_letters([Y|Password], NewPassword, X, Y) -> swap_letters(Password, [X|NewPassword], X, Y);
swap_letters([Z|Password], NewPassword, X, Y) -> swap_letters(Password, [Z|NewPassword], X, Y).

move_position(Password, X, Y) ->
    LetterX = lists:nth(X+1, Password),
    Removed = Password -- [LetterX],
    {Before, After} = lists:split(Y, Removed),
    Before ++ [LetterX] ++ After.

undo_rotate_letter(Password, X) ->
    Rotated = rotate_direction(Password, left, 1),
    check_rotation(Rotated, Password, X, 0).

check_rotation(Rotated, Password, X, Rotations) ->
    TryRotate = rotate_letter(Rotated, X),
    case TryRotate =:= Password of
        true -> Rotated;
        false ->
            case Rotations =:= 4 of
                true -> check_rotation(rotate_direction(Rotated, left, 2), Password, X, Rotations+1);
                false -> check_rotation(rotate_direction(Rotated, left, 1), Password, X, Rotations+1)
            end
    end.

rotate_letter(Password, X) ->
    Index = string:chr(Password, X) - 1,
    FirstRotation = rotate_direction(Password, right, 1),
    case Index >= 4 of
        true -> rotate_direction(FirstRotation, right, Index+1);
        false -> rotate_direction(FirstRotation, right, Index)
    end.

rotate_direction(Password, left, Steps) ->
    AdjustedSteps = Steps rem length(Password),
    {Wrapped, Rotated} = lists:split(AdjustedSteps, Password),
    Rotated ++ Wrapped;
rotate_direction(Password, right, Steps) ->
    AdjustedSteps = (length(Password) - Steps) rem length(Password),
    {Rotated, Wrapped} = lists:split(AdjustedSteps, Password),
    Wrapped ++ Rotated.
