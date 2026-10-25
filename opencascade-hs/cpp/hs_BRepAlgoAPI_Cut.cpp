#include <BRepAlgoAPI_Cut.hxx>
#include "hs_Exception.h"
#include "hs_BRepAlgoAPI_Cut.h"

#include <TopoDS_Shape.hxx>

BRepAlgoAPI_Cut * hs_new_BRepAlgoAPI_Cut_fromShapes(
        TopoDS_Shape * a, TopoDS_Shape * b,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [a, b]{
        return new BRepAlgoAPI_Cut(*a, *b);
    });
}

void hs_delete_BRepAlgoAPI_Cut(BRepAlgoAPI_Cut * builder){
    delete builder;
}
