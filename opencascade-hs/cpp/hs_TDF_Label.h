#ifndef HS_TDF_LABEL_H
#define HS_TDF_LABEL_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

void hs_delete_TDF_Label(TDF_Label * theLabel);

bool hs_TDF_Label_isNull(TDF_Label * theLabel);

#ifdef __cplusplus
}
#endif

#endif // HS_TDF_LABEL_H