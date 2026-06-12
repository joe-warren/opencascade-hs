#include <Geom_BezierCurve.hxx>
#include <NCollection_Array1.hxx>
#include <gp_Pnt.hxx>
#include <Standard_Handle.hxx>
#include "hs_Exception.h"
#include "hs_Geom_BezierCurve.h"

Geom_BezierCurve * hs_new_Geom_BezierCurve_fromPnts(
        ARRAY_1(gp_Pnt) * pnts,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [pnts]{
            return new Geom_BezierCurve(*pnts);
        }
    );
}

Handle(Geom_BezierCurve) * hs_Geom_BezierCurve_toHandle(Geom_BezierCurve * curve){
    return new Handle(Geom_BezierCurve)(new Geom_BezierCurve(*curve));
}

int hs_Geom_BezierCurve_nbPoles(Handle(Geom_BezierCurve)* h){
    return (*h)->NbPoles();
}

gp_Pnt * hs_Geom_BezierCurve_pole(
        Handle(Geom_BezierCurve)* h, int index,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [h, index]{
            return new gp_Pnt((*h)->Pole(index));
        }
    );
}

bool hs_Geom_BezierCurve_isRational(Handle(Geom_BezierCurve) *h) {
    return (*h)->IsRational();
}

void hs_delete_Handle_Geom_BezierCurve(Handle(Geom_BezierCurve)* h){
    delete h;
}

void hs_delete_Geom_BezierCurve(Geom_BezierCurve * curve){
    delete curve;
}

void hs_Geom_BezierCurve_segment(
        Handle(Geom_BezierCurve) *h, double u1, double u2,
        HSExceptionType* exType, void ** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [h, u1, u2]{
            (*h)->Segment(u1, u2);
        }
    );
}