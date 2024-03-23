#ifndef HS_RWOBJ_CAFREADER_H
#define HS_RWOBJ_CAFREADER_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

RWObj_CafReader * hs_new_RWObj_CafReader(void);

void hs_RWObj_CafReader_setSinglePrecision(RWObj_CafReader *reader, bool isSingle);

void hs_delete_RWObj_CafReader(RWObj_CafReader *reader);

#ifdef __cplusplus
}
#endif

#endif // HS_RWOBJ_CAFREADER_H