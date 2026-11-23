#include <ShapeBuild_ReShape.hxx>
#include <Standard_Handle.hxx>
#include <TopAbs_ShapeEnum.hxx>
#include <TopoDS_Shape.hxx>
#include "hs_Exception.h"
#include "hs_ShapeBuild_ReShape.h"

void hs_delete_Handle_ShapeBuild_ReShape(Handle(ShapeBuild_ReShape) * reShape){
    delete reShape;
}

TopoDS_Shape * hs_ShapeBuild_ReShape_apply(
        Handle(ShapeBuild_ReShape) * reShape, TopoDS_Shape * shape, TopAbs_ShapeEnum until,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(exType, exPtr, [reShape, shape, until]{
        return new TopoDS_Shape((*reShape)->Apply(*shape, until));
    });
}

bool hs_ShapeBuild_ReShape_isRecorded(Handle(ShapeBuild_ReShape) * reShape, TopoDS_Shape * shape){
    return (*reShape)->IsRecorded(*shape);
}
