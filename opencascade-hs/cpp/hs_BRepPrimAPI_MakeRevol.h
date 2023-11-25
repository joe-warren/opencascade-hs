#ifndef HS_BREPPRIMAPI_MAKEREVOL_H
#define HS_BREPPRIMAPI_MAKEREVOL_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepPrimAPI_MakeRevol * hs_new_BRepPrimAPI_MakeRevol_fromShapeAndAx1(TopoDS_Shape * shape, gp_Ax1 * axis, bool copy);

void hs_delete_BRepPrimAPI_MakeRevol(BRepPrimAPI_MakeRevol * builder);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPPRIMAPI_MAKEREVOL_H
