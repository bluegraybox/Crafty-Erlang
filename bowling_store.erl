-module(bowling_store).
-import(bowling_game).

-export([init/0, append/3]).


loop(GameData) ->
    receive {From, {append, Player, RollText}} ->
        {Roll, _} = string:to_integer(RollText),
        NewGameData = dict:append(Player, Roll, GameData),
        {ok, Rolls} = dict:find(Player, NewGameData),
        Score = bowling_game:score(Rolls),
        From ! Score,
        loop(NewGameData)  % updated dictionary
    end.

init() ->
    Data = dict:new(),
    Start = fun() -> loop(Data) end,
    spawn(Start).

append(Player, RollText, Pid) ->
    Pid ! {self(), {append, Player, RollText}},
    receive Resp -> Resp end.

