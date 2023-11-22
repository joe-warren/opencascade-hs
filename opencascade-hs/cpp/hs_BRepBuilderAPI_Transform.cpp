#include <BRepBuilderAPI_Transform.hxx>
#include <TopoDS_Shape.hxx>
#include "hs_BRepBuilderAPI_Transform.h"

TopoDS_Shape * hs_BRepBuilderAPI_Transform_transform(TopoDS_Shape * shape, gp_Trsf * trsf, bool copy){
    auto builder = BRepBuilderAPI_Transform(*shape, *trsf, copy);
    return new TopoDS_Shape(builder.Shape());
}


