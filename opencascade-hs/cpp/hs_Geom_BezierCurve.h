#ifndef HS_GEOM_BEZIERCURVE_H
#define HS_GEOM_BEZIERCURVE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

Geom_BezierCurve * hs_new_Geom_BezierCurve_fromPnts(ARRAY_1(gp_Pnt) * pnts);

Handle(Geom_BezierCurve) * hs_Geom_BezierCurve_toHandle(Geom_BezierCurve * curve);

int hs_Geom_BezierCurve_nbPoles(Handle(Geom_BezierCurve)* h);

gp_Pnt * hs_Geom_BezierCurve_pole(Handle(Geom_BezierCurve)* h, int index);

bool hs_Geom_BezierCurve_isRational(Handle(Geom_BezierCurve) *h);

void hs_delete_Handle_Geom_BezierCurve(Handle(Geom_BezierCurve)* h);

void hs_delete_Geom_BezierCurve(Geom_BezierCurve * curve);

#ifdef __cplusplus
}
#endif

#endif // HS_GEOM_BEZIERCURVE_H
