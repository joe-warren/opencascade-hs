#include <ShapeConstruct_Curve.hxx>
#include <Geom_Curve.hxx>
#include "hs_ShapeConstruct_Curve.h"

ShapeConstruct_Curve * hs_new_ShapeConstruct_Curve(){
    return new ShapeConstruct_Curve();
}

void hs_delete_ShapeConstruct_Curve(ShapeConstruct_Curve* shapeConstruct){
    delete shapeConstruct;
}

Handle(Geom_BSplineCurve) * hs_ShapeConstruct_Curve_convertToBSpline(
        ShapeConstruct_Curve* shapeConstruct,
        Handle(Geom_Curve)* curve, 
        double first,
        double last,
        double precision
    ){
        return new opencascade::handle(shapeConstruct->ConvertToBSpline(*curve, first, last, precision));
}
