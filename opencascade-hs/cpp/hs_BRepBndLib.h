#ifndef HS_BREPBNDLIB_H
#define HS_BREPBNDLIB_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

void hs_BRepBndLib_add(TopoDS_Shape * shape, Bnd_Box * box, bool useTriangulation);

void hs_BRepBndLib_addOptimal(TopoDS_Shape * shape, Bnd_Box * box, bool useTriangulation, bool useShapeTolerance);

void hs_BRepBndLib_addOBB(TopoDS_Shape *shape, Bnd_OBB * obb, bool isTriangulationUsed, bool isOptimal, bool isShapeToleranceUsed);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPBNDLIB_H
