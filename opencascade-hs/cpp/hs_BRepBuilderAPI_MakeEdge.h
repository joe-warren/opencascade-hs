#ifndef HS_BREPBUILDERAPI_MAKEEDGE_H
#define HS_BREPBUILDERAPI_MAKEEDGE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

TopoDS_Edge * hs_BRepBuilderAPI_MakeEdge_fromVertices(
    TopoDS_Vertex *a, TopoDS_Vertex *b, 
    HSExceptionType* exType,
    void** exPtr
);

TopoDS_Edge * hs_BRepBuilderAPI_MakeEdge_fromPnts(
    gp_Pnt *a, gp_Pnt *b, 
    HSExceptionType* exType,
    void** exPtr
);

TopoDS_Edge * hs_BRepBuilderAPI_MakeEdge_fromCurve(
    Handle(Geom_Curve) * curve, 
    HSExceptionType* exType,
    void** exPtr
);

TopoDS_Edge * hs_BRepBuilderAPI_MakeEdge_fromCurveAndParameters(
    Handle(Geom_Curve) * curve, double a, double b,
    HSExceptionType* exType,
    void** exPtr
);

TopoDS_Edge * hs_BRepBuilderAPI_MakeEdge_fromCurveAndVertices(
    Handle(Geom_Curve) * curve, TopoDS_Vertex* a, TopoDS_Vertex* b,
    HSExceptionType* exType,
    void** exPtr
);

TopoDS_Edge * hs_BRepBuilderAPI_MakeEdge_fromCurveAndPnts(
    Handle(Geom_Curve) * curve, gp_Pnt* a, gp_Pnt* b,
    HSExceptionType* exType,
    void** exPtr
);

TopoDS_Edge * hs_BRepBuilderAPI_MakeEdge_fromCurveVerticesAndParameters(
    Handle(Geom_Curve) * curve, TopoDS_Vertex* a, TopoDS_Vertex* b, double ap, double bp, 
    HSExceptionType* exType,
    void** exPtr
);

TopoDS_Edge * hs_BRepBuilderAPI_MakeEdge_fromCurvePntsAndParameters(
    Handle(Geom_Curve) * curve, gp_Pnt* a, gp_Pnt* b, double ap, double bp,
    HSExceptionType* exType,
    void** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPBUILDERAPI_MAKEEDGE_H
