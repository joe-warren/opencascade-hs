#include <BRepAlgoAPI_Common.hxx>
#include "hs_Exception.h"
#include "hs_BRepAlgoAPI_Common.h"

#include <TopoDS_Shape.hxx>

TopoDS_Shape * hs_BRepAlgoAPI_Common(
        TopoDS_Shape * a, TopoDS_Shape * b,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [a, b]{
        auto builder = BRepAlgoAPI_Common(*a, *b);
        return new TopoDS_Shape(builder.Shape());
    });
}