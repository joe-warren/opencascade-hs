#include <BRepAlgoAPI_Cut.hxx>
#include "hs_BRepAlgoAPI_Cut.h"

#include <TopoDS_Shape.hxx>

TopoDS_Shape * hs_BRepAlgoAPI_Cut(TopoDS_Shape * a, TopoDS_Shape * b){
    auto builder = BRepAlgoAPI_Cut(*a, *b);
    return new TopoDS_Shape(builder.Shape());
}