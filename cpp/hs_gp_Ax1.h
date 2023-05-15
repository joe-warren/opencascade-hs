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

bool hs_gp_Ax1_IsCoaxial(gp_Ax1* axis1, gp_Ax1* axis2, double angularTolerance, double linearTolerance);

bool hs_gp_Ax1_IsNormal(gp_Ax1* axis1, gp_Ax1* axis2, double angularTolerance);

bool hs_gp_Ax1_IsOpposite(gp_Ax1* axis1, gp_Ax1* axis2, double angularTolerance);

bool hs_gp_Ax1_IsParallel(gp_Ax1* axis1, gp_Ax1* axis2, double angularTolerance);

double hs_gp_Ax1_Angle(gp_Ax1* axis1, gp_Ax1* axis2);

void hs_gp_Ax1_Reverse(gp_Ax1* ax1);

gp_Ax1 * hs_gp_Ax1_Reversed(gp_Ax1* ax1);

void hs_gp_Ax1_Mirror(gp_Ax1* ax1, gp_Ax1* mirror);

gp_Ax1 * hs_gp_Ax1_Mirrored(gp_Ax1* ax1, gp_Ax1* mirror);

void hs_gp_Ax1_MirrorAboutPnt(gp_Ax1* ax1, gp_Pnt* mirror);

gp_Ax1 * hs_gp_Ax1_MirroredAboutPnt(gp_Ax1* ax1, gp_Pnt* mirror);

void hs_gp_Ax1_MirrorAboutAx2(gp_Ax1* ax1, gp_Ax2* mirror);

gp_Ax1 * hs_gp_Ax1_MirroredAboutAx2(gp_Ax1* ax1, gp_Ax2* mirror);

void hs_gp_Ax1_Rotate(gp_Ax1* ax1, gp_Ax1* axisOfRotation, double angle);

gp_Ax1 * hs_gp_Ax1_Rotated(gp_Ax1* ax1, gp_Ax1* axisOfRotation, double angle);

void hs_gp_Ax1_Scale(gp_Ax1* ax1, gp_Pnt* origin, double amount);

gp_Ax1 * hs_gp_Ax1_Scaled(gp_Ax1* ax1, gp_Pnt* origin, double amount);

void hs_gp_Ax1_Transform(gp_Ax1* ax1, gp_Trsf* trsf);

gp_Ax1 * hs_gp_Ax1_Transformed(gp_Ax1* ax1, gp_Trsf* trsf);

void hs_gp_Ax1_Translate(gp_Ax1* ax1, gp_Vec* vec);

gp_Ax1 * hs_gp_Ax1_Translated(gp_Ax1* ax1, gp_Vec* vec);

void hs_gp_Ax1_TranslateRelative(gp_Ax1* ax1, gp_Pnt* from, gp_Pnt* to);

gp_Ax1 * hs_gp_Ax1_TranslatedRelative(gp_Ax1* ax1, gp_Pnt* from, gp_Pnt* to);

#ifdef __cplusplus
}
#endif

#endif // HS_GP_AX1_H
