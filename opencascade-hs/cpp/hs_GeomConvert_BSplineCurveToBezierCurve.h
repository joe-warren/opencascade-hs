#ifndef HS_GEOM_CONVERT_BSPLINE_CURVE_TO_BEZIER_CURVE_H
#define HS_GEOM_CONVERT_BSPLINE_CURVE_TO_BEZIER_CURVE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

GeomConvert_BSplineCurveToBezierCurve * hs_new_GeomConvert_BSplineCurveToBezierCurve_fromHandle (Handle(Geom_BSplineCurve) *basisCurve);

void hs_delete_GeomConvert_BSplineCurveToBezierCurve(GeomConvert_BSplineCurveToBezierCurve* ptr);

int hs_GeomConvert_BSplineCurveToBezierCurve_nbArcs(GeomConvert_BSplineCurveToBezierCurve* ptr);

Handle(Geom_BezierCurve) * hs_GeomConvert_BSplineCurveToBezierCurve_arc(GeomConvert_BSplineCurveToBezierCurve * ptr, int n);

#ifdef __cplusplus
}
#endif

#endif // HS_GEOM_CONVERT_BSPLINE_CURVE_TO_BEZIER_CURVE_H