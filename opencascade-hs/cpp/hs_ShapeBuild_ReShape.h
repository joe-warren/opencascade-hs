#ifndef HS_SHAPEBUILD_RESHAPE_H
#define HS_SHAPEBUILD_RESHAPE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

void hs_delete_Handle_ShapeBuild_ReShape(Handle(ShapeBuild_ReShape) * reShape);

TopoDS_Shape * hs_ShapeBuild_ReShape_apply(
    Handle(ShapeBuild_ReShape) * reShape, TopoDS_Shape * shape, TopAbs_ShapeEnum until,
    HSExceptionType* exType, void ** exPtr
);

bool hs_ShapeBuild_ReShape_isRecorded(Handle(ShapeBuild_ReShape) * reShape, TopoDS_Shape * shape);

#ifdef __cplusplus
}
#endif

#endif // HS_SHAPEBUILD_RESHAPE_H
