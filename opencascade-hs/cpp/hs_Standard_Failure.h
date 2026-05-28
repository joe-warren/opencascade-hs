#ifndef HS_STANDARD_FAILURE_H
#define HS_STANDARD_FAILURE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

void hs_delete_Standard_Failure(Standard_Failure * ex);

char * hs_Standard_Failure_GetStackString(Standard_Failure * ex);

#ifdef __cplusplus
}
#endif

#endif // HS_STANDARD_FAILURE_H
