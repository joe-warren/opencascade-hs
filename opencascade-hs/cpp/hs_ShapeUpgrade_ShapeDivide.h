#ifndef HS_SHAPEUPGRADE_SHAPEDIVIDE_H
#define HS_SHAPEUPGRADE_SHAPEDIVIDE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

bool hs_ShapeUpgrade_ShapeDivide_perform(
    ShapeUpgrade_ShapeDivide * divide, bool newContext,
    HSExceptionType* exType, void ** exPtr
);

TopoDS_Shape * hs_ShapeUpgrade_ShapeDivide_result(
    ShapeUpgrade_ShapeDivide * divide,
    HSExceptionType* exType, void ** exPtr
);

Handle(ShapeBuild_ReShape) * hs_ShapeUpgrade_ShapeDivide_getContext(ShapeUpgrade_ShapeDivide * divide);

void hs_ShapeUpgrade_ShapeDivide_setPrecision(ShapeUpgrade_ShapeDivide * divide, double precision);

void hs_ShapeUpgrade_ShapeDivide_setMaxTolerance(ShapeUpgrade_ShapeDivide * divide, double maxTolerance);

void hs_ShapeUpgrade_ShapeDivide_setMinTolerance(ShapeUpgrade_ShapeDivide * divide, double minTolerance);

#ifdef __cplusplus
}
#endif

#endif // HS_SHAPEUPGRADE_SHAPEDIVIDE_H
