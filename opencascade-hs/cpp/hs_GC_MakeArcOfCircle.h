#ifndef HS_GC_MAKEARCOFCIRCLE_H
#define HS_GC_MAKEARCOFCIRCLE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

Handle(Geom_TrimmedCurve) * hs_GC_MakeArcOfCircle_from3Pnts(
    gp_Pnt * a, gp_Pnt * b, gp_Pnt * c,
    HSExceptionType* exType, void ** exPtr
);

Handle(Geom_TrimmedCurve) * hs_GC_MakeArcOfCircle_fromPntsAndVec(
    gp_Pnt * a, gp_Vec * b, gp_Pnt * c,
    HSExceptionType* exType, void ** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_GC_MAKEARCOFCIRCLE_H
