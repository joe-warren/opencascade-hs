#ifndef HS_RWGLTF_CAFWRITER_H
#define HS_RWGLTF_CAFWRITER_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

RWGltf_CafWriter * hs_new_RWGltf_CafWriter(char * theFile, bool isBinary);

void hs_delete_RWGltf_CafWriter(RWGltf_CafWriter * theWriter);

void hs_RWGltf_CafWriter_Perform(RWGltf_CafWriter * theWriter, Handle(TDocStd_Document) * theDocument, TColStd_IndexedDataMapOfStringString * theFileInfo, Message_ProgressRange * theProgress);

#ifdef __cplusplus
}
#endif

#endif // HS_RWGLTF_CAFWRITER_H