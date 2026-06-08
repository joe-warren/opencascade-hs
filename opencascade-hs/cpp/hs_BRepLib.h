#ifndef HS_BREPLIB_H
#define HS_BREPLIB_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

bool hs_BRepLib_orientClosedSolid(
    TopoDS_Solid * solid,
    HSExceptionType* exType, void ** exPtr
);

bool hs_BRepLib_buildCurve3d(
    TopoDS_Edge* edge, double tolerance, GeomAbs_Shape continuity, int maxDegree, int maxSegment,
    HSExceptionType* exType, void ** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPLIB_H
