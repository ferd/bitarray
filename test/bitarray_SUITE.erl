-module(bitarray_SUITE).
-export([all/0]).
-export([prop/1]).

all() -> [prop].

prop(_Config) ->
    [] = proper:module(bitarray_prop).
