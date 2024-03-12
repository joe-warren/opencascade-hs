
#ifndef HS_STLAPI_READER_H
#define HS_STLAPI_READER_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif


StlAPI_Reader * hs_new_StlAPI_Reader();

void hs_delete_StlAPI_Reader(StlAPI_Reader * reader);

bool hs_StlAPI_Reader_read(StlAPI_Reader * reader, TopoDS_Shape * shape, char* filename);

#ifdef __cplusplus
}
#endif

#endif // HS_STLAPI_READER_H
