#!/usr/local/bin/escript

main([]) ->
    inets:start(),
    next_roll().

next_roll() ->
    Line = io:get_line("Next> "),
    case string:tokens(Line, " \t\n") of
        ["q"] ->
            io:format("Thanks for playing!~n");
        [Player, RollText] ->
            Score = get_score(Player, RollText),
            io:format("New score for ~s: ~p~n", [Player, Score]),
            next_roll()
    end.

get_score(Player, RollText) ->
    Url = io_lib:format("http://localhost:8000/add/~s/~s", [Player, RollText]),
    {ok, {_Status, _Header, Content}} = httpc:request(Url),
    {Score, _} = string:to_integer(Content),
    Score.

