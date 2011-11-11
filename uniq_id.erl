-module(uniq_id).
-export([init/0, next/0]).

init() ->
    Pid = spawn(fun() -> loop(1) end),
    register(uniq_id_service, Pid).

next() ->
    uniq_id_service ! self(),
    receive Resp -> Resp end.

 %% private
loop(Id) ->
    receive Pid ->
        Pid ! Id
    end,
    loop(Id + 1).  % recurse with the new value

