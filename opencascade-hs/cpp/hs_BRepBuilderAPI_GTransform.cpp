#include <BRepBuilderAPI_GTransform.hxx>
#include <TopoDS_Shape.hxx>
#include "hs_Exception.h"
#include "hs_BRepBuilderAPI_GTransform.h"

BRepBuilderAPI_GTransform * hs_new_BRepBuilderAPI_GTransform_fromShapeAndGTrsf(
        TopoDS_Shape * shape, gp_GTrsf * trsf, bool copy,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [shape, trsf, copy]{
        return new BRepBuilderAPI_GTransform(*shape, *trsf, copy);
    });
}

void hs_delete_BRepBuilderAPI_GTransform(BRepBuilderAPI_GTransform * builder){
    delete builder;
}
