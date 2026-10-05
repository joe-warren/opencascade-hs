#include <BRepBuilderAPI_Transform.hxx>
#include <TopoDS_Shape.hxx>
#include "hs_Exception.h"
#include "hs_BRepBuilderAPI_Transform.h"

BRepBuilderAPI_Transform * hs_new_BRepBuilderAPI_Transform_fromShapeTrsfAndCopy(
        TopoDS_Shape * shape, gp_Trsf * trsf, bool copy,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [shape, trsf, copy]{
        return new BRepBuilderAPI_Transform(*shape, *trsf, copy);
    });
}

void hs_delete_BRepBuilderAPI_Transform(BRepBuilderAPI_Transform * builder){
    delete builder;
}
