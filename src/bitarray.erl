-module(bitarray).
-export([new/1, new/2, sub/2, update/3]).
-on_load(init/0).

init() ->
    case code:priv_dir(ossp_uuid) of
        {error, bad_name} ->
            erlang:load_nif(filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "bitarray"]), []);
        Dir ->
            erlang:load_nif(filename:join([Dir, "bitarray"]), [])
    end.

new(_) -> exit(nif_not_loaded).

new(_,_) -> exit(nif_not_loaded).

sub(_,_) -> exit(nif_not_loaded).

update(_,_,_) -> exit(nif_not_loaded).

