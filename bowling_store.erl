-module(bowling_store).
-import(bowling_game).

-export([init/0, append/3, find/2]).


loop(Dict) ->
    receive {From, Req} ->
        case Req of
            {find, Key} ->
                Value = dict_find(Key, Dict),
                Score = bowling_game:score(Value),
                From ! Score,
                loop(Dict);
            {append, Key, Value} ->
                NewDict = dict:append(Key, Value, Dict),
                Rolls = dict_find(Key, NewDict),
                Score = bowling_game:score(Rolls),
                From ! Score,
                loop(NewDict)  % updated dictionary
        end
    end.

dict_find(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, Value} -> Value;
        error -> []
    end.

init() ->
    Data = dict:new(),
    Start = fun() -> loop(Data) end,
    spawn(Start).

append(Key, Value, Pid) ->
    Pid ! {self(), {append, Key, Value}},
    receive Resp -> Resp end.

find(Key, Pid) ->
    Pid ! {self(), {find, Key}},
    receive Resp -> Resp end.

