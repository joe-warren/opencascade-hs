#ifndef HS_GC_SEGMENT_H
#define HS_GC_SEGMENT_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

Handle(Geom_TrimmedCurve) * hs_GC_MakeSegment_fromPnts(gp_Pnt* a, gp_Pnt* b);


#ifdef __cplusplus
}
#endif

#endif // HS_GC_MAKESEGMENT_H
