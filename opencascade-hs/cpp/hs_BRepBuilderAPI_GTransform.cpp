#include <BRepBuilderAPI_GTransform.hxx>
#include <TopoDS_Shape.hxx>
#include "hs_BRepBuilderAPI_GTransform.h"

TopoDS_Shape * hs_BRepBuilderAPI_GTransform_gtransform(TopoDS_Shape * shape, gp_GTrsf * trsf, bool copy){
    auto builder = BRepBuilderAPI_GTransform(*shape, *trsf, copy);
    return new TopoDS_Shape(builder.Shape());
}


