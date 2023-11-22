#include <BRepAlgoAPI_Fuse.hxx>
#include "hs_BRepAlgoAPI_Fuse.h"

#include <TopoDS_Shape.hxx>

TopoDS_Shape * hs_BRepAlgoAPI_Fuse(TopoDS_Shape * a, TopoDS_Shape * b){
    auto builder = BRepAlgoAPI_Fuse(*a, *b);
    return new TopoDS_Shape(builder.Shape());
}