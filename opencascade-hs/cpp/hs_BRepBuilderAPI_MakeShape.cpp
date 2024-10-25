#include <opencascade/BRepBuilderAPI_MakeShape.hxx>
#include <opencascade/TopoDS_Shape.hxx>
#include "hs_BRepBuilderAPI_MakeShape.h"

TopoDS_Shape * hs_BRepBuilderAPI_MakeShape_shape(BRepBuilderAPI_MakeShape* builder){
    return new TopoDS_Shape(builder->Shape());
}