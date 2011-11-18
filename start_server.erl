#!/usr/local/bin/escript


main([]) ->
    case os:getenv("SPOOKY_DIR") of
        false -> 
            io:format("SPOOKY_DIR not set~n");
        SpookyDir ->
            main([SpookyDir])
    end;
main([SpookyDir]) ->
    %% Add spooky and its dependencies to the code path.
    true = code:add_path(SpookyDir ++ "/ebin"),
    Deps = filelib:wildcard(SpookyDir ++ "/deps/*/ebin"),
    ok = code:add_paths(Deps),

    %% Compile our modules, just to be safe.
    c:c(bowling_game),
    c:c(bowling_store),
    c:c(bowling_web),

    spooky:start_link(bowling_web),
    io:format("Started spooky~n"),

    io:get_line("Return to exit...  "),
    spooky:stop(),
    io:format("Stopped spooky~n").

