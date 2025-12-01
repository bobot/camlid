#include "./mylib_stub.h"

static void combine_data_or_status(camlid_result *dst, intptr_t *status, intptr_t *data)
{
    if (*status)
    {
        camlid_mk_result_Error(dst, status);
    }
    else {
        camlid_mk_result_Data(dst, data);
    };
}