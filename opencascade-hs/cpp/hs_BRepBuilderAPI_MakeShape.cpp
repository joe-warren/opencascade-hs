#include <BRepBuilderAPI_MakeShape.hxx>
#include <TopoDS_Shape.hxx>
#include "hs_BRepBuilderAPI_MakeShape.h"

TopoDS_Shape * hs_BRepBuilderAPI_MakeShape_shape(BRepBuilderAPI_MakeShape* builder){
    return new TopoDS_Shape(builder->Shape());
}

void hs_BRepBuilderAPI_MakeShape_build(BRepBuilderAPI_MakeShape* builder){
    builder->Build();
}

bool hs_BRepBuilderAPI_MakeShape_isDone(BRepBuilderAPI_MakeShape* builder){
    return builder->IsDone();
}