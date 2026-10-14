#ifndef HS_STANDARD_FAILURE_H
#define HS_STANDARD_FAILURE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

void hs_delete_Standard_Failure(HS_STANDARD_FAILURE * ex);

char * hs_Standard_Failure_GetTypeName(HS_STANDARD_FAILURE * ex);

char * hs_Standard_Failure_GetMessageString(HS_STANDARD_FAILURE * ex);

char * hs_Standard_Failure_GetStackString(HS_STANDARD_FAILURE * ex);

#ifdef __cplusplus
}
#endif

#endif // HS_STANDARD_FAILURE_H
