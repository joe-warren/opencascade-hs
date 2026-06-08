#include <GC_MakeSegment.hxx>
#include <Standard_Failure.hxx>
#include "hs_Exception.h"
#include "hs_GC_MakeSegment.h"

Handle(Geom_TrimmedCurve) * hs_GC_MakeSegment_fromPnts(
        gp_Pnt* a, gp_Pnt* b,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [a, b]{
            return new opencascade::handle(GC_MakeSegment(*a, *b).Value());
        }
    );
}
