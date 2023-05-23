#include <BRepBuilderAPI_MakeEdge.hxx>
#include <TopoDS_Edge.hxx>
#include "hs_BRepBuilderAPI_MakeEdge.h"

TopoDS_Edge * hs_BRepBuilderAPI_MakeEdge_fromVertices(TopoDS_Vertex *a, TopoDS_Vertex *b){
    return new TopoDS_Edge(BRepBuilderAPI_MakeEdge(*a, *b));
}

TopoDS_Edge * hs_BRepBuilderAPI_MakeEdge_fromPnts(gp_Pnt *a, gp_Pnt *b){
    return new TopoDS_Edge(BRepBuilderAPI_MakeEdge(*a, *b));
}

TopoDS_Edge * hs_BRepBuilderAPI_MakeEdge_fromCurve(Handle(Geom_Curve) * curve){
    return new TopoDS_Edge(BRepBuilderAPI_MakeEdge(*curve));
}

TopoDS_Edge * hs_BRepBuilderAPI_MakeEdge_fromCurveAndParameters(Handle(Geom_Curve) * curve, double a, double b){
    return new TopoDS_Edge(BRepBuilderAPI_MakeEdge(*curve, a, b));
}

TopoDS_Edge * hs_BRepBuilderAPI_MakeEdge_fromCurveAndVertices(Handle(Geom_Curve) * curve, TopoDS_Vertex* a, TopoDS_Vertex* b){
    return new TopoDS_Edge(BRepBuilderAPI_MakeEdge(*curve, *a, *b));
}

TopoDS_Edge * hs_BRepBuilderAPI_MakeEdge_fromCurveAndPnts(Handle(Geom_Curve) * curve, gp_Pnt* a, gp_Pnt* b){
    return new TopoDS_Edge(BRepBuilderAPI_MakeEdge(*curve, *a, *b));
}

TopoDS_Edge * hs_BRepBuilderAPI_MakeEdge_fromCurveVerticesAndParameters(Handle(Geom_Curve) * curve, TopoDS_Vertex* a, TopoDS_Vertex* b, double ap, double bp){
    return new TopoDS_Edge(BRepBuilderAPI_MakeEdge(*curve, *a, *b, ap, bp));
}


TopoDS_Edge * hs_BRepBuilderAPI_MakeEdge_fromCurvePntsAndParameters(Handle(Geom_Curve) * curve, gp_Pnt* a, gp_Pnt* b, double ap, double bp){
    return new TopoDS_Edge(BRepBuilderAPI_MakeEdge(*curve, *a, *b, ap, bp));
}
