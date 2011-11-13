#!/usr/local/bin/escript

main([]) -> main(["web_tests.data"]);
main([Filename]) ->
    inets:start(),
    Tests = load_tests(Filename),
    Passed = Failed = 0,
    test(Tests, Passed, Failed).

test([], Passed, 0) ->
    io:format("Passed! (~p tests)~n", [Passed]);

test([], Passed, Failed) ->
    io:format("Failed! Passed ~p, Failed ~p~n", [Passed, Failed]);

test([{Player, Roll, Expected} | Tests], Passed, Failed) ->
    case get_score(Player, Roll) of
        Expected ->
            io:format("."),
            test(Tests, Passed + 1, Failed);
        Actual ->
            io:format("Failed: expected=~p, actual=~p~n", [Expected, Actual]),
            test(Tests, Passed, Failed + 1)
    end.

get_score(Player, Roll) ->
    Url = io_lib:format("http://localhost:8000/add/~p/~p", [Player, Roll]),
    {ok, {_Status, _Header, Content}} = httpc:request(Url),
    {Score, _} = string:to_integer(Content),
    Score.

load_tests(Filename) ->
    {ok, Handle} = file:open(Filename, [read]),
    {ok, Tests} = io:read(Handle, ""),
    file:close(Handle),
    Tests.

