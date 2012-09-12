#include "erl_nif.h"

typedef struct {
	unsigned size;
	char *set;
} bitarray;

ErlNifResourceType* BITARRAY;
ERL_NIF_TERM atom_true;
ERL_NIF_TERM atom_false;

// Only free up the bit sets; the bitarray record
// is actually allocated differently and should
// be freed by the VM.
void free_bitarray(ErlNifEnv* env, void* obj) {
	bitarray * bits = (bitarray *)obj;
	enif_free(bits->set);
}

// Initializes data on the first load ever
// Return value of 0 indicates success.
// Docs: http://erlang.org/doc/man/erl_nif.html#load
static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info) {
	// own data type for the bitarray struct
	const char* mod = "bitarray";
	const char* name = "Bitarray";
	int flags = ERL_NIF_RT_CREATE;

	// instantiate the BITARRAY resource type
	BITARRAY = enif_open_resource_type(
			env,
			mod,
			name,
			free_bitarray,
			flags,
			NULL);
	if (BITARRAY == NULL) return -1;

	atom_true = enif_make_atom(env, "true");
	atom_false = enif_make_atom(env, "false");

	return 0;
}

// Called when changing versions of the C code for a module's NIF
// implementation. We have no priv data to rewrite or change in
// this function, so we just return 0 to indicate success.
// Docs: http://erlang.org/doc/man/erl_nif.html#upgrade
static int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info) {
	return 0;
}

// Called when the library is unloaded.
// No return value, no private stuff to purge.
// Docs: http://erlang.org/doc/man/erl_nif.html#load
static void unload(ErlNifEnv* env, void* priv) {
	return;
}

// Returns a new bit array, initialized to 0, and 0-indexed.
// Expected arguments:
// - integer: number of bits in the array
//
// Docs: http://erlang.org/doc/man/erl_nif.html#ErlNifFunc
static ERL_NIF_TERM bitarray_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	unsigned size, cells, i;
	char bytemask;
	bitarray * bits;
	ERL_NIF_TERM ret;

	if (!enif_get_uint(env, argv[0], &size))
		return enif_make_badarg(env);

	bits = enif_alloc_resource(BITARRAY, sizeof(bitarray));
	if (bits == NULL)
		return enif_make_badarg(env);
	
	// give the allocation away to Erlang
	ret = enif_make_resource(env, bits);
	enif_release_resource(bits);

	cells = (size + ((1 << 3) - 1)) >> 3;
	bits->size = size;
	bits->set = enif_alloc(sizeof(char)*cells); // manual management for this

	// initialize to 0
	if (argc == 2 && enif_compare(argv[1], atom_true)==0) {
		bytemask = ~0;
	} else {
		bytemask = 0;
	}
	for (i=0; i < cells; i++) {
		bits->set[i] = bytemask;
	}

	return ret;
}

// Returns the value of the bit at position X, as an atom (true | false)
// Expected arguments:
// - bitarray: opaque bitarray as returned by bitarray_new through Erlang
// - integer: 0-based index of the array -- bit to fetch the value of
static ERL_NIF_TERM bitarray_sub(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	unsigned pos;
	bitarray * bits;
	char bytemask;

	if (!enif_get_resource(env, argv[0], BITARRAY, (void**) &bits))
		return enif_make_badarg(env);
	if (!enif_get_uint(env, argv[1], &pos))
		return enif_make_badarg(env);
	if (pos >= bits->size)
		return enif_make_badarg(env);
	
	bytemask = 1 << (pos & ((1 << 3) - 1));
	if ((bits->set[pos >> 3] & bytemask) == 0) {
		return atom_false;
	} else {
		return atom_true;
	}
}

// Updates the bit of the bitarray at position X to the boolean value
// equivalent to (true | false) as it was passed.
// Expected arguments:
// - bitarray: opaque bitarray as returned by bitarray_new through Erlang
// - integer: 0-based index of the array -- bit to fetch the value of
// - true|false: atom, value to switch to.
static ERL_NIF_TERM bitarray_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	unsigned pos;
	char bytemask;
	ERL_NIF_TERM val;
	bitarray * bits;

	if (argc != 3)
		return enif_make_badarg(env);
	if (!enif_get_resource(env, argv[0], BITARRAY, (void**) &bits))
		return enif_make_badarg(env);
	if (!enif_get_uint(env, argv[1], &pos))
		return enif_make_badarg(env);
	if (pos >= bits->size)
		return enif_make_badarg(env);

	val = argv[2];
	bytemask = 1 << (pos & ((1 << 3) -1));
	if (enif_compare(val, atom_true)==0) {
		bits->set[pos >> 3] |= bytemask;
	} else if (enif_compare(val, atom_false)==0) {
		bits->set[pos >> 3] &= ~bytemask;
	} else {
		return enif_make_badarg(env);
	}
	return argv[0];
}

static ErlNifFunc nif_funcs[] = {
	{"new", 1, bitarray_new},
	{"new", 2, bitarray_new},
	{"sub", 2, bitarray_sub},
	{"update", 3, bitarray_update}
};

// Initialize this NIF library.
// Args: (MODULE, ErlNifFunc funcs[], load, reload, upgrade, unload)
//  (reload deprecated and is now NULL)
// Docs: http://erlang.org/doc/man/erl_nif.html#ERL_NIF_INIT
ERL_NIF_INIT(bitarray, nif_funcs, &load, NULL, &upgrade, &unload);
