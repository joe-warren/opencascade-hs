#ifndef HS_GCPNTS_ABSCISSAPOINT_H
#define HS_GCPNTS_ABSCISSAPOINT_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

GCPnts_AbscissaPoint * hs_new_GCPnts_AbscissaPoint(BRepAdaptor_Curve * curve, double abscissa, double u0);

void hs_delete_GCPnts_AbscissaPoint(GCPnts_AbscissaPoint * abscissaPoint);

double hs_GCPnts_AbscissaPoint_parameter(GCPnts_AbscissaPoint * abscissaPoint);

bool hs_GCPnts_AbscissaPoint_isDone(GCPnts_AbscissaPoint * abscissaPoint);

#ifdef __cplusplus
}
#endif

#endif // HS_GCPNTS_ABSCISSAPOINT_H
