#ifndef HS_GEOM_ADAPTOR_CURVE_H
#define HS_GEOM_ADAPTOR_CURVE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

GeomAdaptor_Curve * hs_new_GeomAdaptor_Curve_fromHandle(Handle(Geom_Curve) *curve);

void hs_delete_GeomAdaptor_Curve(GeomAdaptor_Curve * adaptor);

double hs_GeomAdaptor_Curve_firstParameter(GeomAdaptor_Curve * adaptor);

double hs_GeomAdaptor_Curve_lastParameter(GeomAdaptor_Curve * adaptor);

Handle(Geom_Curve)* hs_GeomAdaptor_Curve_curve(GeomAdaptor_Curve * adaptor);

#ifdef __cplusplus
}
#endif

#endif // HS_GEOM_ADAPTOR_CURVE_H
