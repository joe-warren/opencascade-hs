#include <BRepPrimAPI_MakeRevol.hxx>
#include "hs_Exception.h"
#include "hs_BRepPrimAPI_MakeRevol.h"

BRepPrimAPI_MakeRevol * hs_new_BRepPrimAPI_MakeRevol_fromShapeAndAx1(
        TopoDS_Shape * shape, gp_Ax1 * axis, bool copy,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [shape, axis, copy]{
        return new BRepPrimAPI_MakeRevol(*shape, *axis, copy);
    });
}

void hs_delete_BRepPrimAPI_MakeRevol(BRepPrimAPI_MakeRevol * builder){
    delete builder;
}