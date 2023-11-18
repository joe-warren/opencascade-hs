#ifndef HS_BREPPRIMAPI_MAKEBOX_H
#define HS_BREPPRIMAPI_MAKEBOX_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepPrimAPI_MakeBox * hs_new_BRepPrimAPI_MakeBox_fromPnts(gp_Pnt * a, gp_Pnt * b);

void hs_delete_BRepPrimAPI_MakeBox(BRepPrimAPI_MakeBox* builder);

TopoDS_Shape * hs_BRepPrimAPI_MakeBox_Solid(BRepPrimAPI_MakeBox* builder);

TopoDS_Shell * hs_BRepPrimAPI_MakeBox_Shell(BRepPrimAPI_MakeBox* builder);


#ifdef __cplusplus
}
#endif

#endif // HS_BREPPRIMAPI_MAKEBOX_H
