-module(bowling_game).
-export([score/1, test/0]).

score(Rolls) -> score(1, 0, Rolls).

score(11, Score, _BonusRolls) -> Score;

score(Frame, Score, [10|Rest]) ->
    score(Frame + 1, Score + 10 + strike_bonus(Rest), Rest);

score(Frame, Score, [First,Second|Rest]) when (First + Second == 10) ->
    score(Frame + 1, Score + 10 + spare_bonus(Rest), Rest);

score(Frame, Score, [First,Second|Rest]) ->
    score(Frame + 1, Score + First + Second, Rest);

score(_Frame, Score, [First]) -> Score + First;
score(_Frame, Score, []) -> Score.


spare_bonus([]) -> 0;
spare_bonus([First|_Rest]) -> First.

strike_bonus([]) -> 0;
strike_bonus([Only]) -> Only;
strike_bonus([10,Second|_Rest]) -> 10 + Second;
strike_bonus([First,Second|_Rest]) -> First + Second.


test() ->
    test([
        [0,   [0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0]],
        [20,  [1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1]],
        [6,   [1,1, 1,1, 1,1]], %% incomplete
        [18,  [1,1, 6,4, 3]], %% incomplete w/ spare
        [150, [5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5,5, 5]],
        [47,  [1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 10, 10 ,9]],
        [173, [7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 7,3, 10]],
        [300, [10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10]],
        [280, [10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  5]],  % incomplete
        [300, [10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10, 10, 10, 10]], %% extras
        [240, [10,  10,  10,  0,0, 10,  10,  10,  10,  10,  10,  10,  10]],
        [245, [10,  10,  10,  10,  10,  10,  10,  10,  10,  1,1]]]).

test(Tests) -> test(0, 0, Tests).
test(Pass, 0, []) -> io:fwrite("~nPassed! ~p tests~n", [Pass]);
test(Pass, Fail, []) -> io:fwrite("~nFailed! ~p fail, ~p pass~n", [Fail, Pass]);
test(Pass, Fail, [[Expected, Rolls]|Tests]) ->
    case score(Rolls) of
        Expected -> io:fwrite("."),
            test(Pass + 1, Fail, Tests);
        Scored -> io:fwrite("~nFail: expected=~p, scored=~p~n", [Expected, Scored]),
            test(Pass, Fail + 1, Tests)
    end.

