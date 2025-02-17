#include <GeomConvert_ApproxCurve.hxx>
#include <Geom_Curve.hxx>
#include "hs_GeomConvert_ApproxCurve.h"

GeomConvert_ApproxCurve * hs_new_GeomConvert_ApproxCurve_fromCurveToleranceOrderSegmentsAndDegree(
        Handle(Geom_Curve) *curve, 
        double tolerance, 
        GeomAbs_Shape order, 
        int maxSegments, 
        int maxDegree
    ) {
        return new GeomConvert_ApproxCurve(*curve, tolerance, order, maxSegments, maxDegree);
}

void hs_delete_GeomConvert_ApproxCurve(GeomConvert_ApproxCurve * approxCurve){
    delete approxCurve;
}

Handle(Geom_BSplineCurve) * hs_GeomConvert_ApproxCurve_curve(GeomConvert_ApproxCurve * approxCurve){
    return new opencascade::handle(approxCurve->Curve());
}

bool hs_GeomConvert_ApproxCurve_isDone(GeomConvert_ApproxCurve * approxCurve){
    return approxCurve->IsDone();
}

bool hs_GeomConvert_ApproxCurve_hasResult(GeomConvert_ApproxCurve * approxCurve){
    return approxCurve->HasResult();
}
