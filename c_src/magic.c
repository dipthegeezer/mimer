
#include "erl_nif.h"
#include <stdlib.h>
#include "magic.h"

static ErlNifResourceType* magic_RESOURCE;

typedef struct
{
} magic_handle;

// Prototypes
ERL_NIF_TERM magic_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM magic_myfunction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"new", 0, magic_new},
    {"magic_file", 1, magic_file}
    {"magic_buffer", 1, magic_buffer}
};

ERL_NIF_TERM magic_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    magic_handle* handle = enif_alloc_resource(env,
                                               magic_RESOURCE,
                                               sizeof(magic_handle));
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(env, handle);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}


ERL_NIF_TERM magic_file(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM magic_buffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "ok");
}

static void magic_resource_cleanup(ErlNifEnv* env, void* arg)
{
    // Delete any dynamically allocated memory stored in magic_handle
    // magic_handle* handle = (magic_handle*)arg;
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    magic_RESOURCE = enif_open_resource_type(env, "magic_resource",
                                                  &magic_resource_cleanup,
                                                  ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
                                                  0);
    return 0;
}

ERL_NIF_INIT(magic, nif_funcs, &on_load, NULL, NULL, NULL);
