#ifndef HS_GEOM_CONVERT_APPROX_CURVE_H
#define HS_GEOM_CONVERT_APPROX_CURVE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

GeomConvert_ApproxCurve * hs_new_GeomConvert_ApproxCurve_fromCurveToleranceOrderSegmentsAndDegree(
        Handle(Geom_Curve) *curve, 
        double tolerance, 
        GeomAbs_Shape order, 
        int maxSegments, 
        int maxDegree
    );

void hs_delete_GeomConvert_ApproxCurve(GeomConvert_ApproxCurve * approxCurve);

Handle(Geom_BSplineCurve) * hs_GeomConvert_ApproxCurve_curve(GeomConvert_ApproxCurve * approxCurve);

bool hs_GeomConvert_ApproxCurve_isDone(GeomConvert_ApproxCurve * approxCurve);

bool hs_GeomConvert_ApproxCurve_hasResult(GeomConvert_ApproxCurve * approxCurve);

#ifdef __cplusplus
}
#endif

#endif // HS_GEOM_CONVERT_APPROX_CURVE_H
