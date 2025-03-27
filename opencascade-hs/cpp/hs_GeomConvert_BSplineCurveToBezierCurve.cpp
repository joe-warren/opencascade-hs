#include <GeomConvert_BSplineCurveToBezierCurve.hxx>
#include "hs_types.h"
#include "hs_GeomConvert_BSplineCurveToBezierCurve.h"

GeomConvert_BSplineCurveToBezierCurve * hs_new_GeomConvert_BSplineCurveToBezierCurve_fromHandle (Handle(Geom_BSplineCurve) *basisCurve){
    return new GeomConvert_BSplineCurveToBezierCurve(*basisCurve);
}

GeomConvert_BSplineCurveToBezierCurve * hs_new_GeomConvert_BSplineCurveToBezierCurve_fromHandleParametersAndTolerance (
    Handle(Geom_BSplineCurve) *basisCurve,
    double firstParameter, 
    double secondParameter,
    double tolerance ){
    return new GeomConvert_BSplineCurveToBezierCurve(*basisCurve, firstParameter, secondParameter, tolerance);
}

void hs_delete_GeomConvert_BSplineCurveToBezierCurve(GeomConvert_BSplineCurveToBezierCurve* ptr){
    delete ptr;
}

int hs_GeomConvert_BSplineCurveToBezierCurve_nbArcs(GeomConvert_BSplineCurveToBezierCurve* ptr){
    return ptr->NbArcs();
}

Handle(Geom_BezierCurve) * hs_GeomConvert_BSplineCurveToBezierCurve_arc(GeomConvert_BSplineCurveToBezierCurve * ptr, int n){
    return new opencascade::handle(ptr->Arc(n));
} 