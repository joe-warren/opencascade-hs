#ifndef HS_GP_AX2D_H
#define HS_GP_AX2D_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

gp_Ax2d* hs_new_gp_Ax2d(gp_Pnt2d* location, gp_Dir2d* direction);

void hs_delete_gp_Ax2d(gp_Ax2d* ax2d);

gp_Dir2d * hs_gp_Ax2d_Direction(gp_Ax2d* ax2d);

gp_Pnt2d * hs_gp_Ax2d_Location(gp_Ax2d* ax2d);

void hs_gp_Ax2d_SetDirection(gp_Ax2d* ax2d, gp_Dir2d* direction);

void hs_gp_Ax2d_SetLocation(gp_Ax2d* ax2d, gp_Pnt2d* origin);

bool hs_gp_Ax2d_IsCoaxial(gp_Ax2d* axis1, gp_Ax2d* axis2, double angularTolerance, double linearTolerance);

bool hs_gp_Ax2d_IsNormal(gp_Ax2d* axis1, gp_Ax2d* axis2, double angularTolerance);

bool hs_gp_Ax2d_IsOpposite(gp_Ax2d* axis1, gp_Ax2d* axis2, double angularTolerance);

bool hs_gp_Ax2d_IsParallel(gp_Ax2d* axis1, gp_Ax2d* axis2, double angularTolerance);

double hs_gp_Ax2d_Angle(gp_Ax2d* axis1, gp_Ax2d* axis2);

void hs_gp_Ax2d_Reverse(gp_Ax2d* ax2d);

gp_Ax2d * hs_gp_Ax2d_Reversed(gp_Ax2d* ax2d);

void hs_gp_Ax2d_Mirror(gp_Ax2d* ax2d, gp_Ax2d* mirror);

gp_Ax2d * hs_gp_Ax2d_Mirrored(gp_Ax2d* ax2d, gp_Ax2d* mirror);

void hs_gp_Ax2d_MirrorAboutPnt2d(gp_Ax2d* ax2d, gp_Pnt2d* mirror);

gp_Ax2d * hs_gp_Ax2d_MirroredAboutPnt2d(gp_Ax2d* ax2d, gp_Pnt2d* mirror);

void hs_gp_Ax2d_Rotate(gp_Ax2d* ax2d, gp_Pnt2d* axisOfRotation, double angle);

gp_Ax2d * hs_gp_Ax2d_Rotated(gp_Ax2d* ax2d, gp_Pnt2d* axisOfRotation, double angle);

void hs_gp_Ax2d_Scale(gp_Ax2d* ax2d, gp_Pnt2d* origin, double amount);

gp_Ax2d * hs_gp_Ax2d_Scaled(gp_Ax2d* ax2d, gp_Pnt2d* origin, double amount);

void hs_gp_Ax2d_Transform(gp_Ax2d* ax2d, gp_Trsf2d* trsf);

gp_Ax2d * hs_gp_Ax2d_Transformed(gp_Ax2d* ax2d, gp_Trsf2d* trsf);

void hs_gp_Ax2d_Translate(gp_Ax2d* ax2d, gp_Vec2d* vec);

gp_Ax2d * hs_gp_Ax2d_Translated(gp_Ax2d* ax2d, gp_Vec2d* vec);

void hs_gp_Ax2d_TranslateRelative(gp_Ax2d* ax2d, gp_Pnt2d* from, gp_Pnt2d* to);

gp_Ax2d * hs_gp_Ax2d_TranslatedRelative(gp_Ax2d* ax2d, gp_Pnt2d* from, gp_Pnt2d* to);

#ifdef __cplusplus
}
#endif

#endif // HS_GP_AX2D_H
