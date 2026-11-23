#include <ShapeUpgrade_ShapeDivide.hxx>
#include <ShapeBuild_ReShape.hxx>
#include <Standard_Handle.hxx>
#include <TopoDS_Shape.hxx>
#include "hs_Exception.h"
#include "hs_ShapeUpgrade_ShapeDivide.h"

bool hs_ShapeUpgrade_ShapeDivide_perform(
        ShapeUpgrade_ShapeDivide * divide, bool newContext,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleExWithDefault(
        exType,
        exPtr,
        [divide, newContext]{
            return static_cast<bool>(divide->Perform(newContext));
        },
        false
    );
}

TopoDS_Shape * hs_ShapeUpgrade_ShapeDivide_result(
        ShapeUpgrade_ShapeDivide * divide,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(exType, exPtr, [divide]{
        return new TopoDS_Shape(divide->Result());
    });
}

Handle(ShapeBuild_ReShape) * hs_ShapeUpgrade_ShapeDivide_getContext(ShapeUpgrade_ShapeDivide * divide){
    return new Handle(ShapeBuild_ReShape)(divide->GetContext());
}

void hs_ShapeUpgrade_ShapeDivide_setPrecision(ShapeUpgrade_ShapeDivide * divide, double precision){
    divide->SetPrecision(precision);
}

void hs_ShapeUpgrade_ShapeDivide_setMaxTolerance(ShapeUpgrade_ShapeDivide * divide, double maxTolerance){
    divide->SetMaxTolerance(maxTolerance);
}

void hs_ShapeUpgrade_ShapeDivide_setMinTolerance(ShapeUpgrade_ShapeDivide * divide, double minTolerance){
    divide->SetMinTolerance(minTolerance);
}
