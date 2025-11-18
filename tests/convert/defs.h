#include "mylib_stub.h"

static void combine_data_or_status(camlid_result *dst, camlid_int *status, camlid_int *data)
{
    if (*status)
    {
        camlid_mk_result_Error(dst, status);
    }
    else {
        camlid_mk_result_Data(dst, data);
    };
};