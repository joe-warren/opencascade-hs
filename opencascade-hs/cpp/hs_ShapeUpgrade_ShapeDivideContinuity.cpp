#include <ShapeUpgrade_ShapeDivideContinuity.hxx>
#include <GeomAbs_Shape.hxx>
#include <TopoDS_Shape.hxx>
#include "hs_Exception.h"
#include "hs_ShapeUpgrade_ShapeDivideContinuity.h"

ShapeUpgrade_ShapeDivideContinuity * hs_new_ShapeUpgrade_ShapeDivideContinuity_fromShape(
        TopoDS_Shape * shape,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(exType, exPtr, [shape]{
        return new ShapeUpgrade_ShapeDivideContinuity(*shape);
    });
}

void hs_delete_ShapeUpgrade_ShapeDivideContinuity(ShapeUpgrade_ShapeDivideContinuity * divide){
    delete divide;
}

void hs_ShapeUpgrade_ShapeDivideContinuity_setTolerance(ShapeUpgrade_ShapeDivideContinuity * divide, double tolerance){
    divide->SetTolerance(tolerance);
}

void hs_ShapeUpgrade_ShapeDivideContinuity_setTolerance2d(ShapeUpgrade_ShapeDivideContinuity * divide, double tolerance){
    divide->SetTolerance2d(tolerance);
}

void hs_ShapeUpgrade_ShapeDivideContinuity_setBoundaryCriterion(ShapeUpgrade_ShapeDivideContinuity * divide, GeomAbs_Shape criterion){
    divide->SetBoundaryCriterion(criterion);
}

void hs_ShapeUpgrade_ShapeDivideContinuity_setPCurveCriterion(ShapeUpgrade_ShapeDivideContinuity * divide, GeomAbs_Shape criterion){
    divide->SetPCurveCriterion(criterion);
}

void hs_ShapeUpgrade_ShapeDivideContinuity_setSurfaceCriterion(ShapeUpgrade_ShapeDivideContinuity * divide, GeomAbs_Shape criterion){
    divide->SetSurfaceCriterion(criterion);
}
