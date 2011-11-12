-module(bowling_web).
-behaviour(spooky).
-export([init/1, get/2]).
-import(bowling_store).

%%% A REST web service for tracking bowling scores.

%% Uses the Spooky web framework; check it out from GitHub, build it, then run
%%     start_server.erl <Spooky dir>
%% to compile this code and start the Spooky server on http://localhost:8000/.

init([])->
    register(store, bowling_store:init()),
    [{port, 8000}].


get(Req, ["add", Player, RollText])->
    {Roll, _} = string:to_integer(RollText),
    Score = bowling_store:append(Player, Roll, store),
    Req:ok(io_lib:format("~p", [Score]));

get(Req, [])->
    get(Req, ["form.html"]);

get(_Req, Path)->
    %% Assume these are static pages for the UI.
    Filename = filename:join(Path),
    case file:read_file(Filename) of
        {ok, PageBytes} -> {200, binary_to_list(PageBytes)};
        {error, Reason} -> {404, Reason}
    end.

