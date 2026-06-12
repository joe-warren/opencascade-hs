#include <GCPnts_AbscissaPoint.hxx>
#include <BRepAdaptor_Curve.hxx>
#include "hs_GCPnts_AbscissaPoint.h"

GCPnts_AbscissaPoint * hs_new_GCPnts_AbscissaPoint(BRepAdaptor_Curve * curve, double abscissa, double u0){
    return new GCPnts_AbscissaPoint(*curve, abscissa, u0);
}

void hs_delete_GCPnts_AbscissaPoint(GCPnts_AbscissaPoint * abscissaPoint){
    delete abscissaPoint;
}

double hs_GCPnts_AbscissaPoint_parameter(GCPnts_AbscissaPoint * abscissaPoint){
    return abscissaPoint->Parameter();
}

bool hs_GCPnts_AbscissaPoint_isDone(GCPnts_AbscissaPoint * abscissaPoint){
    return abscissaPoint->IsDone();
}
