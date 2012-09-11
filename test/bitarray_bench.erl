-module(bitarray_bench).
-export([run/2]).

run(Iterations, Init) ->
    lists:seq(1, Iterations),
    Res = [run(empty, Iterations, fun() -> ok end, fun(_Arr) -> ok end),
           run(hipe_bifs, Iterations,
               fun() -> hipe_bifs:bitarray(50, Init) end,
               fun(Arr) -> hipe_bifs:bitarray_update(Arr, 20, true) end),
           run(hipe_bifs, Iterations,
               fun() -> hipe_bifs:bitarray(50, Init) end,
               fun(Arr) -> hipe_bifs:bitarray_sub(Arr, 20) end),
           run(bitarray, Iterations,
               fun() -> bitarray:new(50, Init) end,
               fun(Arr) -> bitarray:update(Arr, 20, true) end),
           run(bitarray, Iterations,
               fun() -> bitarray:new(50, Init) end,
               fun(Arr) -> bitarray:sub(Arr, 20) end)],
    [Empty, BifW, BifR, NifW, NifR] = Res,
    io:format("Iterations: ~p~n", [Iterations]),
    io:format("hipe write res: ~p microsecs~n",[BifW-Empty]),
    io:format("nif write res: ~p microsecs~n",[NifW-Empty]),
    io:format("hipe read res: ~p microsecs~n",[BifR-Empty]),
    io:format("nif read res: ~p microsecs~n",[NifR-Empty]).

run(_Mod, Iterations, Init, Up) ->
    Arr = Init(),
    T1 = os:timestamp(),
    run(Arr, Iterations, Up),
    timer:now_diff(os:timestamp(), T1).

run(_, 0, _) -> ok;
run(Arr, Iterations, Up) ->
    Up(Arr),
    run(Arr, Iterations - 1, Up).
    %Total = lists:sum([element(1, timer:tc(Up, [Arr])) || _ <- lists:seq(1,Iterations)]),
    %io:format("~p res (~p iterations): ~pmicrosecs~n",[Mod, Iterations, Total]).
    %Total.
