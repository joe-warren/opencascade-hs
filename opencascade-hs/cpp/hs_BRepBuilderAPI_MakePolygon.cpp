#include <BRepBuilderAPI_MakePolygon.hxx>
#include <TopoDS_Wire.hxx>
#include "hs_BRepBuilderAPI_MakePolygon.h"

TopoDS_Wire * hs_BRepBuilderAPI_MakePolygon_from3Pnts(gp_Pnt * n1, gp_Pnt *n2, gp_Pnt * n3, bool close){
    auto builder = BRepBuilderAPI_MakePolygon(*n1, *n2, *n3, close);
    return new TopoDS_Wire(builder.Wire());
}