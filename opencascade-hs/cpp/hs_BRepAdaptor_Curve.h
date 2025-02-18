#ifndef HS_BREP_ADAPTOR_CURVE_H
#define HS_BREP_ADAPTOR_CURVE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepAdaptor_Curve * hs_new_BRepAdaptor_Curve_fromEdge(TopoDS_Edge * edge);

void hs_delete_BRepAdaptor_Curve(BRepAdaptor_Curve * curve);

GeomAbs_CurveType hs_BRepAdaptor_Curve_curveType(BRepAdaptor_Curve* curve);

Handle(Geom_BezierCurve) * hs_BRepAdaptor_Curve_bezier(BRepAdaptor_Curve * curve);

Handle(Geom_BSplineCurve) * hs_BRepAdaptor_Curve_bspline(BRepAdaptor_Curve * curve);

GeomAdaptor_Curve * hs_BRepAdaptor_Curve_curve(BRepAdaptor_Curve * curve);

#ifdef __cplusplus
}
#endif

#endif // HS_BREP_ADAPTOR_CURVE_H
