#ifndef HS_BREPPRIMAPI_MAKETORUS_H
#define HS_BREPPRIMAPI_MAKETORUS_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepPrimAPI_MakeTorus * hs_new_BRepPrimAPI_MakeTorus_fromRadii(double major, double minor);

void hs_delete_BRepPrimAPI_MakeTorus(BRepPrimAPI_MakeTorus * makeTorus);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPPRIMAPI_MAKETORUS_H
