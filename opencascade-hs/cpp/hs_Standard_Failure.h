#ifndef HS_STANDARD_FAILURE_H
#define HS_STANDARD_FAILURE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

void hs_delete_Standard_Failure(Standard_Failure * ex);

char * hs_Standard_Failure_getMessageString(Standard_Failure *ex);

void hs_Standard_Failure_catch(void (*inner)(), void(*handler)(Standard_Failure *));

#ifdef __cplusplus
}
#endif

#endif // HS_SHAPEFIX_SOLID_H
