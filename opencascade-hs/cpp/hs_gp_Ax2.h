#ifndef HS_GP_AX2_H
#define HS_GP_AX2_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

gp_Ax2 * hs_new_gp_Ax2(gp_Pnt * origin, gp_Dir * north, gp_Dir * vX);

void hs_delete_gp_Ax2(gp_Ax2* ax2);

gp_Ax2 * hs_new_gp_Ax2_autoX(gp_Pnt * origin, gp_Dir * north);

gp_Dir * hs_gp_Ax2_Direction(gp_Ax2* ax2);

gp_Pnt * hs_gp_Ax2_Location(gp_Ax2* ax2);

gp_Dir * hs_gp_Ax2_XDirection(gp_Ax2* ax2);

gp_Dir * hs_gp_Ax2_YDirection(gp_Ax2* ax2);

gp_Ax1 * hs_gp_Ax2_Axis(gp_Ax2* ax2);

void hs_gp_Ax2_SetDirection(gp_Ax2* ax2, gp_Dir* direction);

void hs_gp_Ax2_SetLocation(gp_Ax2* ax2, gp_Pnt* origin);

void hs_gp_Ax2_SetXDirection(gp_Ax2* ax2, gp_Dir* direction);

void hs_gp_Ax2_SetYDirection(gp_Ax2* ax2, gp_Dir* direction);

void hs_gp_Ax2_SetAxis(gp_Ax2* ax2, gp_Ax1* ax1);

bool hs_gp_Ax2_IsCoplanar(gp_Ax2* axis1, gp_Ax2* axis2, double linearTolerance, double angularTolerance);

bool hs_gp_Ax2_IsCoplanarWithAx1(gp_Ax2* axis1, gp_Ax1* axis2, double linearTolerance, double angularTolerance);

void hs_gp_Ax2_Mirror(gp_Ax2* ax2, gp_Ax2* mirror);

gp_Ax2 * hs_gp_Ax2_Mirrored(gp_Ax2* ax2, gp_Ax2* mirror);

void hs_gp_Ax2_MirrorAboutPnt(gp_Ax2* ax2, gp_Pnt* mirror);

gp_Ax2 * hs_gp_Ax2_MirroredAboutPnt(gp_Ax2* ax2, gp_Pnt* mirror);

void hs_gp_Ax2_MirrorAboutAx1(gp_Ax2* ax2, gp_Ax1* mirror);

gp_Ax2 * hs_gp_Ax2_MirroredAboutAx1(gp_Ax2* ax2, gp_Ax1* mirror);

void hs_gp_Ax2_Rotate(gp_Ax2* ax2, gp_Ax1* axisOfRotation, double angle);

gp_Ax2 * hs_gp_Ax2_Rotated(gp_Ax2* ax2, gp_Ax1* axisOfRotation, double angle);

void hs_gp_Ax2_Scale(gp_Ax2* ax2, gp_Pnt* origin, double amount);

gp_Ax2 * hs_gp_Ax2_Scaled(gp_Ax2* ax2, gp_Pnt* origin, double amount);

void hs_gp_Ax2_Transform(gp_Ax2* ax2, gp_Trsf* trsf);

gp_Ax2 * hs_gp_Ax2_Transformed(gp_Ax2* ax2, gp_Trsf* trsf);

void hs_gp_Ax2_Translate(gp_Ax2* ax2, gp_Vec* vec);

gp_Ax2 * hs_gp_Ax2_Translated(gp_Ax2* ax2, gp_Vec* vec);

void hs_gp_Ax2_TranslateRelative(gp_Ax2* ax2, gp_Pnt* from, gp_Pnt* to);

gp_Ax2 * hs_gp_Ax2_TranslatedRelative(gp_Ax2* ax2, gp_Pnt* from, gp_Pnt* to);

#ifdef __cplusplus
}
#endif

#endif // HS_GP_AX2_H
