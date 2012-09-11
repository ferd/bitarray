-module(bitarray_bench).
-export([run/2]).

run(Iterations, Init) ->
    Res = [run(empty, Iterations, fun() -> ok end, fun(_Arr) -> ok end),
           run(hipe_bifs, Iterations,
               fun() -> hipe_bifs:bitarray(50, Init) end,
               fun(Arr) ->
                        Val = hipe_bifs:bitarray_sub(Arr, 20),
                        hipe_bifs:bitarray_update(Arr, 20, not Val)
                end),
           run(bitarray, Iterations,
               fun() -> bitarray:new(50, Init) end,
               fun(Arr) ->
                        Val = bitarray:sub(Arr, 20),
                        bitarray:update(Arr, 20, not Val)
                end)],
    [Empty, Bif, Nif] = Res,
    io:format("Iterations: ~p~n", [Iterations]),
    io:format("hipe res: ~p microsecs~n",[Bif-Empty]),
    io:format("nif res: ~p microsecs~n",[Nif-Empty]).

run(_Mod, Iterations, Init, Up) ->
    Arr = Init(),
    Total = lists:sum([element(1, timer:tc(Up, [Arr])) || _ <- lists:seq(1,Iterations)]),
    %io:format("~p res (~p iterations): ~pmicrosecs~n",[Mod, Iterations, Total]).
    Total.
