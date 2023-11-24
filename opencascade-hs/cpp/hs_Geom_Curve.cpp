#include <Geom_Curve.hxx>
#include <gp_Pnt.hxx>
#include "hs_Geom_Curve.h"

void hs_delete_Handle_Geom_Curve(Handle(Geom_Curve) * handle){
    delete handle;
}

gp_Pnt * hs_Geom_Curve_value(Handle(Geom_Curve) * curve, double u){
    return new gp_Pnt((*curve)->Value(u));
}

