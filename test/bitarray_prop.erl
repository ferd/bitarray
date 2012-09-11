-module(bitarray_prop).
-include_lib("proper/include/proper.hrl").
-compile(export_all).

%% the bit array is 0-indexed and respects the given length.
%% However, according to hipe_bifs:bitarray behaviour, when the size
%% is 0, the bitarray is created, but never valid for a sub call.
prop_boundaries() ->
    ?FORALL({Size,Array}, bitarray(),
            begin
                if Size > 1  ->
                    false = bitarray:sub(Array, 0),
                    false = bitarray:sub(Array, 1),
                    false = bitarray:sub(Array, Size-1);
                   Size =:= 1 ->
                    false = bitarray:sub(Array, 0),
                    false = bitarray:sub(Array, Size-1),
                    {'EXIT', {badarg, _}} = (catch bitarray:sub(Array, 1));
                   Size =:= 0 ->
                    {'EXIT', {badarg, _}} = (catch bitarray:sub(Array, Size-1))
                end,
                {'EXIT', {badarg, _}} = (catch bitarray:sub(Array, -1)),
                {'EXIT', {badarg, _}} = (catch bitarray:sub(Array, Size)),
                true
            end).

%% Destructive updates are possible
prop_update_seq() ->
    ?FORALL(Trans, update_seq(),
            begin
                Arr = bitarray:new(2),
                bitarray:update(Arr, 1, true),
                Last = hd(lists:reverse(Trans)),
                [bitarray:update(Arr, 0, Flip) || Flip <- Trans],
                true = bitarray:sub(Arr, 1),
                Last = bitarray:sub(Arr, 0),
                true
            end).

%%% GENERATORS
bitarray() -> bitarray(false).

bitarray(Init) ->
    ?LET(Size, non_neg_integer(), {Size, bitarray:new(Size, Init)}).

update_seq() ->
    vector(15, boolean()).
