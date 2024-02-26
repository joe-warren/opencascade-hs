#ifndef HS_GP_XYZ_H
#define HS_GP_XYZ_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

gp_XYZ * hs_new_gp_XYZ();

gp_XYZ * hs_new_gp_XYZ_fromDoubles(double x, double y, double z);

void hs_delete_gp_XYZ(gp_XYZ * xyz);

void hs_gp_XYZ_setX(gp_XYZ * xyz, double x);

void hs_gp_XYZ_setY(gp_XYZ * xyz, double y);

void hs_gp_XYZ_setZ(gp_XYZ * xyz, double z);

double hs_gp_XYZ_x(gp_XYZ * xyz);

double hs_gp_XYZ_y(gp_XYZ * xyz);

double hs_gp_XYZ_z(gp_XYZ * xyz);

#ifdef __cplusplus
}
#endif

#endif // HS_GP_XYZ_H

