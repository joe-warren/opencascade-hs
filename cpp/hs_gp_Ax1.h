#ifndef HS_GP_AX1_H
#define HS_GP_AX1_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

gp_Ax1 * hs_new_gp_Ax1(gp_Pnt * origin, gp_Dir * direction);

void hs_delete_gp_Ax1(gp_Ax1* ax1);

gp_Dir * hs_gp_Ax1_Direction(gp_Ax1* ax1);

gp_Pnt * hs_gp_Ax1_Location(gp_Ax1* ax1);

void hs_gp_Ax1_SetDirection(gp_Ax1* ax1, gp_Dir* direction);

void hs_gp_Ax1_SetLocation(gp_Ax1* ax1, gp_Pnt* origin);

#ifdef __cplusplus
}
#endif

#endif // HS_GP_AX1_H
