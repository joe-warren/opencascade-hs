
#ifndef HS_GEOM_BSPLINECURVE_H
#define HS_GEOM_BSPLINECURVE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

Handle(Geom_BSplineCurve) * hs_Geom_BSplineCurve_toHandle(Geom_BSplineCurve * curve);

void hs_delete_Handle_Geom_BSplineCurve(Handle(Geom_BSplineCurve)* h);

void hs_delete_Geom_BSplineCurve(Geom_BSplineCurve * curve);

#ifdef __cplusplus
}
#endif

#endif // HS_GEOM_BSPLINECURVE_H
