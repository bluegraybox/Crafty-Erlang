-module(bowling_game).
-export([score/1]).

score(Rolls) -> score(Rolls, 1, 0).

score(_BonusRolls, 11, Score) -> Score;

score([10 | Rest], Frame, Score) ->
    score(Rest, Frame + 1, Score + 10 + strike_bonus(Rest));

score([Roll1, Roll2 | Rest], Frame, Score) when (Roll1 + Roll2 == 10) ->
    score(Rest, Frame + 1, Score + 10 + spare_bonus(Rest));

score([Roll1, Roll2 | Rest], Frame, Score) ->
    score(Rest, Frame + 1, Score + Roll1 + Roll2);

score([Roll1], _Frame, Score) -> Score + Roll1;
score([], _Frame, Score) -> Score.


spare_bonus([]) -> 0;
spare_bonus([Bonus1 | _Rest]) -> Bonus1.

strike_bonus([]) -> 0;
strike_bonus([Only]) -> Only;
strike_bonus([10, Bonus2 | _Rest]) -> 10 + Bonus2;
strike_bonus([Bonus1, Bonus2 | _Rest]) -> Bonus1 + Bonus2.

