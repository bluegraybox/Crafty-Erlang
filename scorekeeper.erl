#!/usr/local/bin/escript

-import(bowling_game).

main([]) ->
    next_roll(dict:new()).

next_roll(Dict) ->
    Line = io:get_line("Next> "),
    case string:tokens(Line, " \t\n") of
        ["q"] ->
            io:format("Thanks for playing!~n");
        [Player, RollText] ->
            {Roll, _} = string:to_integer(RollText),
            NewDict = dict:append(Player, Roll, Dict),
            Rolls = dict_find(Player, NewDict),
            Score = bowling_game:score(Rolls),
            io:format("New score for ~s: ~p~n", [Player, Score]),
            next_roll(NewDict)
    end.

dict_find(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, Value} -> Value;
        error -> []
    end.

