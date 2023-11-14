#ifndef HS_BREPPRIMAPI_MAKECYLINDER_H
#define HS_BREPPRIMAPI_MAKECYLINDER_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

TopoDS_Solid * hs_BRepPrimAPI_MakeCylinder_fromRadiusAndHeight(double r, double h);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPPRIMAPI_MAKECYLINDER_H
