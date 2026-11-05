#include <BRepAlgoAPI_Section.hxx>
#include "hs_Exception.h"
#include "hs_BRepAlgoAPI_Section.h"

#include <TopoDS_Shape.hxx>

BRepAlgoAPI_Section * hs_new_BRepAlgoAPI_Section_fromShapes(
        TopoDS_Shape * a, TopoDS_Shape * b,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [a, b]{
        return new BRepAlgoAPI_Section(*a, *b);
    });
}

void hs_delete_BRepAlgoAPI_Section(BRepAlgoAPI_Section * builder){
    delete builder;
}
