
#ifndef HS_STLAPI_WRITER_H
#define HS_STLAPI_WRITER_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

StlAPI_Writer * hs_new_StlAPI_Writer();

void hs_delete_StlAPI_Writer(StlAPI_Writer * writer);

void hs_StlAPI_Writer_setAsciiMode(StlAPI_Writer * writer, bool asciiMode);

void hs_StlAPI_Writer_write(StlAPI_Writer * writer, TopoDS_Shape * shape, char* filename);

#ifdef __cplusplus
}
#endif

#endif // HS_STLAPI_WRITER_H
