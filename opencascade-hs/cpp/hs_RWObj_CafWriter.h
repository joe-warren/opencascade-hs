#ifndef HS_RWOBJ_CAFWRITER_H
#define HS_RWOBJ_CAFWRITER_H

#include "hs_types.h"

#ifdef __cplusplus
    extern "C" {
#endif

RWObj_CafWriter * hs_new_RWObj_CafWriter(char * theFile);

void hs_delete_RWObj_CafWriter(RWObj_CafWriter * theWriter);

void hs_RWObj_CafWriter_Perform(RWObj_CafWriter * theWriter, Handle(TDocStd_Document) * theDocument, TColStd_IndexedDataMapOfStringString * theFileInfo, Message_ProgressRange * theProgress);

#ifdef __cplusplus
    }
#endif

#endif // HS_RWOBJ_CAFWRITER_H