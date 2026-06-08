#ifndef HS_BREPPRIMAPI_MAKESPHERE_H
#define HS_BREPPRIMAPI_MAKESPHERE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

TopoDS_Solid * hs_BRepPrimAPI_MakeSphere_fromRadius(
    double r,
    HSExceptionType* exType,
    void** exPtr
);

TopoDS_Solid * hs_BRepPrimAPI_MakeSphere_fromPntAndRadius(
    gp_Pnt * center, double r,
    HSExceptionType* exType,
    void** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPPRIMAPI_MAKESPHERE_H
