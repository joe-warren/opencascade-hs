#ifndef HS_TDOCSTD_DOCUMENT_H
#define HS_TDOCSTD_DOCUMENT_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

Handle(TDocStd_Document) * hs_new_TDocStd_Document(char * storageFormat);

void hs_delete_TDocStd_Document(Handle(TDocStd_Document) * theDocument);

TDF_Label * hs_TDocStd_Document_main(Handle(TDocStd_Document) *theDocument);

#ifdef __cplusplus
}
#endif

#endif // HS_TDOCSTD_DOCUMENT_H