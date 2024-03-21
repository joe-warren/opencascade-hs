#include <GC_MakeArcOfCircle.hxx>
#include "hs_GC_MakeArcOfCircle.h"

Handle(Geom_TrimmedCurve) * hs_GC_MakeArcOfCircle_from3Pnts(gp_Pnt * a, gp_Pnt * b, gp_Pnt * c){
    return new opencascade::handle(GC_MakeArcOfCircle(*a, *b, *c).Value());
}

Handle(Geom_TrimmedCurve) * hs_GC_MakeArcOfCircle_fromPntsAndVec(gp_Pnt * a, gp_Vec * b, gp_Pnt * c){
    return new opencascade::handle(GC_MakeArcOfCircle(*a, *b, *c).Value());
}

