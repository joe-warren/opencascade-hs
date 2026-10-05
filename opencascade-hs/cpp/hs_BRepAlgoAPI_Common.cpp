#include <BRepAlgoAPI_Common.hxx>
#include "hs_Exception.h"
#include "hs_BRepAlgoAPI_Common.h"

#include <TopoDS_Shape.hxx>

BRepAlgoAPI_Common * hs_new_BRepAlgoAPI_Common_fromShapes(
        TopoDS_Shape * a, TopoDS_Shape * b,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [a, b]{
        return new BRepAlgoAPI_Common(*a, *b);
    });
}

void hs_delete_BRepAlgoAPI_Common(BRepAlgoAPI_Common * builder){
    delete builder;
}
