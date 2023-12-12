#ifndef HS_GEOM_CURVE_H
#define HS_GEOM_CURVE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif
void hs_delete_Handle_Geom_Curve(Handle(Geom_Curve) * handle);

gp_Pnt * hs_Geom_Curve_value(Handle(Geom_Curve) * curve, double u);

gp_Vec * hs_Geom_Curve_dn(Handle (Geom_Curve) * curve, double u, int n);
#ifdef __cplusplus
}
#endif

#endif // HS_GEOM_CURVE_H
