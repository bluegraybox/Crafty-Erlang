-module(kvstore).
-mode(compile).

-export([init/0, append/3, find/2, test/0]).


loop(Dict) ->
    receive {From, Req} ->
        case Req of
            {find, Key} ->
                Value = dict_find(Key, Dict),
                From ! Value,
                loop(Dict);
            {append, Key, Value} ->
                NewDict = dict:append(Key, Value, Dict),
                From ! ok,
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

test() ->
    Pid = init(),
    [] = find(foo, Pid),  % key has no entries initially
    ok = append(foo, bar, Pid),
    [bar] = find(foo, Pid),
    ok = append(foo, baz, Pid),
    [bar, baz] = find(foo, Pid),  % entries append, rather than replace
    [] = find(bar, Pid),  % second key is not connected to first
    Pid2 = init(),
    [] = find(foo, Pid2),  % second store is unconnected to first
    ok.

