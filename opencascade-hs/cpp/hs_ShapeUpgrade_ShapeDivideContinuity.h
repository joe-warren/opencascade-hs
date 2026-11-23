#ifndef HS_SHAPEUPGRADE_SHAPEDIVIDECONTINUITY_H
#define HS_SHAPEUPGRADE_SHAPEDIVIDECONTINUITY_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

ShapeUpgrade_ShapeDivideContinuity * hs_new_ShapeUpgrade_ShapeDivideContinuity_fromShape(
    TopoDS_Shape * shape,
    HSExceptionType* exType, void ** exPtr
);

void hs_delete_ShapeUpgrade_ShapeDivideContinuity(ShapeUpgrade_ShapeDivideContinuity * divide);

void hs_ShapeUpgrade_ShapeDivideContinuity_setTolerance(ShapeUpgrade_ShapeDivideContinuity * divide, double tolerance);

void hs_ShapeUpgrade_ShapeDivideContinuity_setTolerance2d(ShapeUpgrade_ShapeDivideContinuity * divide, double tolerance);

void hs_ShapeUpgrade_ShapeDivideContinuity_setBoundaryCriterion(ShapeUpgrade_ShapeDivideContinuity * divide, GeomAbs_Shape criterion);

void hs_ShapeUpgrade_ShapeDivideContinuity_setPCurveCriterion(ShapeUpgrade_ShapeDivideContinuity * divide, GeomAbs_Shape criterion);

void hs_ShapeUpgrade_ShapeDivideContinuity_setSurfaceCriterion(ShapeUpgrade_ShapeDivideContinuity * divide, GeomAbs_Shape criterion);

#ifdef __cplusplus
}
#endif

#endif // HS_SHAPEUPGRADE_SHAPEDIVIDECONTINUITY_H
