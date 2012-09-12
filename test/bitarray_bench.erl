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
    {HipeInit,NifInit} = bench_init(Iterations, Init),
    [Empty, BifW, BifR, NifW, NifR] = Res,
    io:format("Iterations: ~p~n", [Iterations]),
    io:format("hipe init res: ~p microsecs~n", [HipeInit]),
    io:format("nif init res: ~p microsecs~n", [NifInit]),
    io:format("hipe write res: ~p microsecs~n",[BifW-Empty]),
    io:format("nif write res: ~p microsecs~n",[NifW-Empty]),
    io:format("hipe read res: ~p microsecs~n",[BifR-Empty]),
    io:format("nif read res: ~p microsecs~n",[NifR-Empty]).

run(_Mod, Iterations, Init, Op) ->
    Arr = Init(),
    T1 = os:timestamp(),
    run(Arr, Iterations, Op),
    timer:now_diff(os:timestamp(), T1).

run(_, 0, _) -> ok;
run(Arr, Iterations, Op) ->
    Op(Arr),
    run(Arr, Iterations - 1, Op).

bench_init(N, Init) ->
    {bench_hipe_init(N, Init, os:timestamp()),
     bench_nif_init(N, Init, os:timestamp())}.

bench_hipe_init(0, _, Ts) -> timer:now_diff(os:timestamp(), Ts);
bench_hipe_init(N, Init, Ts) ->
    hipe_bifs:bitarray(300, Init),
    bench_hipe_init(N-1, Init, Ts).

bench_nif_init(0, _, Ts) -> timer:now_diff(os:timestamp(), Ts);
bench_nif_init(N, Init, Ts) ->
    bitarray:new(300, Init),
    bench_nif_init(N-1, Init, Ts).
