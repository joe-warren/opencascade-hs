#include <BRepAlgoAPI_Fuse.hxx>
#include "hs_Exception.h"
#include "hs_BRepAlgoAPI_Fuse.h"

#include <TopoDS_Shape.hxx>

BRepAlgoAPI_Fuse * hs_new_BRepAlgoAPI_Fuse_fromShapes(
        TopoDS_Shape * a, TopoDS_Shape * b,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [a, b]{
        return new BRepAlgoAPI_Fuse(*a, *b);
    });
}

void hs_delete_BRepAlgoAPI_Fuse(BRepAlgoAPI_Fuse * builder){
    delete builder;
}
