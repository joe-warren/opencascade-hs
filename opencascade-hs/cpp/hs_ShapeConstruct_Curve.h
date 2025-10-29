#ifndef HS_SHAPECONSTRUCT_CURVE_H
#define HS_SHAPECONSTRUCT_CURVE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

ShapeConstruct_Curve * hs_new_ShapeConstruct_Curve();

void hs_delete_ShapeConstruct_Curve(ShapeConstruct_Curve* shapeConstruct);

Handle(Geom_BSplineCurve) * hs_ShapeConstruct_Curve_convertToBSpline(
        ShapeConstruct_Curve* shapeConstruct,
        Handle(Geom_Curve)* curve, 
        double first,
        double last,
        double precision
    );

#ifdef __cplusplus
}
#endif

#endif // HS_SHAPECONSTRUCT_CURVE_H