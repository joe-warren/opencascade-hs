#ifndef HS_GP_LIN_H
#define HS_GP_LIN_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

gp_Lin * hs_new_gp_Lin(gp_Pnt * origin, gp_Dir * direction);

gp_Lin * hs_new_gp_Lin_fromAx1(gp_Ax1 * axis);

void hs_delete_gp_Lin(gp_Lin* lin);

gp_Pnt * hs_gp_Lin_Location(gp_Lin* lin);

gp_Dir * hs_gp_Lin_Direction(gp_Lin* lin);

gp_Ax1 * hs_gp_Lin_Position(gp_Lin* lin);

void hs_gp_Lin_SetLocation(gp_Lin* lin, gp_Pnt * pnt);

void hs_gp_Lin_SetDirection(gp_Lin* lin, gp_Dir * dir);

void hs_gp_Lin_SetPosition(gp_Lin* lin, gp_Ax1 * axis);

void hs_gp_Lin_Reverse(gp_Lin* lin);

gp_Lin * hs_gp_Lin_Reversed(gp_Lin* lin);

double hs_gp_Lin_Angle(gp_Lin* lin, gp_Lin* other);

bool hs_gp_Lin_Contains(gp_Lin* lin, gp_Pnt* pnt, double linearTolerance);

double hs_gp_Lin_DistancePnt(gp_Lin* lin, gp_Pnt* pnt);

double hs_gp_Lin_DistanceLin(gp_Lin* lin, gp_Lin* other);

double hs_gp_Lin_SquareDistancePnt(gp_Lin* lin, gp_Pnt* pnt);

double hs_gp_Lin_SquareDistanceLin(gp_Lin* lin, gp_Lin* other);

gp_Lin * hs_gp_Lin_Normal(gp_Lin* lin, gp_Pnt* pnt, HSExceptionType* exType, void** exPtr);

void hs_gp_Lin_Mirror(gp_Lin * lin, gp_Pnt * pnt);

gp_Lin * hs_gp_Lin_Mirrored(gp_Lin * lin, gp_Pnt * pnt);

void hs_gp_Lin_MirrorAboutAx1(gp_Lin * lin, gp_Ax1 * axis);

gp_Lin * hs_gp_Lin_MirroredAboutAx1(gp_Lin * lin, gp_Ax1 * axis);

void hs_gp_Lin_MirrorAboutAx2(gp_Lin * lin, gp_Ax2 * axis);

gp_Lin * hs_gp_Lin_MirroredAboutAx2(gp_Lin * lin, gp_Ax2 * axis);

void hs_gp_Lin_Rotate(gp_Lin * lin, gp_Ax1 * axis, double amount);

gp_Lin * hs_gp_Lin_Rotated(gp_Lin * lin, gp_Ax1 * axis, double amount);

void hs_gp_Lin_Scale(gp_Lin * lin, gp_Pnt * origin, double amount);

gp_Lin * hs_gp_Lin_Scaled(gp_Lin * lin, gp_Pnt * origin, double amount);

void hs_gp_Lin_Transform(gp_Lin * lin, gp_Trsf * trsf);

gp_Lin * hs_gp_Lin_Transformed(gp_Lin * lin, gp_Trsf * trsf);

void hs_gp_Lin_Translate(gp_Lin * lin, gp_Vec * vec);

gp_Lin * hs_gp_Lin_Translated(gp_Lin * lin, gp_Vec * vec);

void hs_gp_Lin_TranslateRelative(gp_Lin * lin, gp_Pnt * from, gp_Pnt * to);

gp_Lin * hs_gp_Lin_TranslatedRelative(gp_Lin * lin, gp_Pnt * from, gp_Pnt * to);

#ifdef __cplusplus
}
#endif

#endif // HS_GP_LIN_H
