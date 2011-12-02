#!/usr/local/bin/escript

-import(bowling_game).

main([]) -> loop(dict:new()).

loop(Dict) ->
    Line = io:get_line("Next> "),
    case string:tokens(Line, " \t\n") of
        ["q"] ->
            io:format("Thanks for playing!~n");
        [Player, RollText] ->
            {Roll, _} = string:to_integer(RollText),
            NewDict = dict:append(Player, Roll, Dict),
            {ok, Rolls} = dict:find(Player, NewDict),
            Score = bowling_game:score(Rolls),
            io:format("New score for ~s: ~p~n", [Player, Score]),
            loop(NewDict)
    end.

