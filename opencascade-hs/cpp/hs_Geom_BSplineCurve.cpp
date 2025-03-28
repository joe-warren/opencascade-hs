#include <Geom_BSplineCurve.hxx>
#include <NCollection_Array1.hxx>
#include <gp_Pnt.hxx>
#include <Standard_Handle.hxx>
#include "hs_Geom_BSplineCurve.h"

Handle(Geom_BSplineCurve) * hs_Geom_BSplineCurve_toHandle(Geom_BSplineCurve * curve){
    return new Handle(Geom_BSplineCurve)(new Geom_BSplineCurve(*curve));
}

void hs_delete_Handle_Geom_BSplineCurve(Handle(Geom_BSplineCurve)* h){
    delete h;
}

void hs_delete_Geom_BSplineCurve(Geom_BSplineCurve * curve){
    delete curve;
}

int hs_Geom_BSplineCurve_nbPoles(Handle(Geom_BSplineCurve)* h){
    return (*h)->NbPoles();
}

gp_Pnt * hs_Geom_BSplineCurve_pole(Handle(Geom_BSplineCurve)* h, int index){
    return new gp_Pnt((*h)->Pole(index));
}

bool hs_Geom_BSplineCurve_isRational(Handle(Geom_BSplineCurve) *h) {
    return (*h)->IsRational();
}

void hs_Geom_BSplineCurve_segment(Handle(Geom_BSplineCurve) *h, double u1, double u2, double confusion){
    (*h)->Segment(u1, u2, confusion);
}
