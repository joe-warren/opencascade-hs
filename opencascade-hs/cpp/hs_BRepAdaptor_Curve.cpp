#include <GeomAbs_CurveType.hxx>
#include <BRepAdaptor_Curve.hxx>
#include "hs_BRepAdaptor_Curve.h"

BRepAdaptor_Curve * hs_new_BRepAdaptor_Curve_fromEdge(TopoDS_Edge * edge){
    return new BRepAdaptor_Curve(*edge);
}

void hs_delete_BRepAdaptor_Curve(BRepAdaptor_Curve * curve){
    delete curve;
}

GeomAbs_CurveType hs_BRepAdaptor_Curve_curveType(BRepAdaptor_Curve* curve){
    return curve->GetType();   
}

Handle(Geom_BezierCurve) * hs_BRepAdaptor_Curve_bezier(BRepAdaptor_Curve * curve){
    return new opencascade::handle(curve->Bezier());
}

Handle(Geom_BSplineCurve) * hs_BRepAdaptor_Curve_bspline(BRepAdaptor_Curve * curve){
    return new opencascade::handle(curve->BSpline());
}