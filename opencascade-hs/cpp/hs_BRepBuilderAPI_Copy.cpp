#include <BRepBuilderAPI_Copy.hxx>
#include "hs_Exception.h"
#include "hs_BRepBuilderAPI_Copy.h"

TopoDS_Shape * hs_BRepBuilderAPI_Copy_copy(
        TopoDS_Shape *shape, bool copyGeom, bool copyMesh,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [shape, copyGeom, copyMesh]{
        return new TopoDS_Shape(BRepBuilderAPI_Copy(*shape, copyGeom, copyMesh).Shape());
    });
}