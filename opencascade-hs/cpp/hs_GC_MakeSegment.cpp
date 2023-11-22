#include <GC_MakeSegment.hxx>
#include "hs_GC_MakeSegment.h"

Handle(Geom_TrimmedCurve) * hs_GC_MakeSegment_fromPnts(gp_Pnt* a, gp_Pnt* b){
    return new opencascade::handle(GC_MakeSegment(*a, *b).Value());
}
