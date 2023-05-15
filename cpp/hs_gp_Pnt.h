#ifndef HS_GP_PNT_H
#define HS_GP_PNT_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

gp_Pnt * hs_new_gp_Pnt(double x, double y, double z);

void hs_delete_gp_Pnt(gp_Pnt* pnt);

double hs_gp_Pnt_X(gp_Pnt * pnt);

double hs_gp_Pnt_Y(gp_Pnt * pnt);

double hs_gp_Pnt_Z(gp_Pnt * pnt);

void hs_gp_Pnt_SetX(gp_Pnt * pnt, double x);

void hs_gp_Pnt_SetY(gp_Pnt * pnt, double y);

void hs_gp_Pnt_SetZ(gp_Pnt * pnt, double z);

double hs_gp_Pnt_Distance(gp_Pnt * a, gp_Pnt * b );

double hs_gp_Pnt_SquareDistance(gp_Pnt * a, gp_Pnt * b );

void hs_gp_Pnt_BaryCenter(gp_Pnt * a, double alpha, gp_Pnt * b, double beta);

bool hs_gp_Pnt_IsEqual(gp_Pnt * a, gp_Pnt * b, double tolerance);

void hs_gp_Pnt_Mirror(gp_Pnt * thePnt, gp_Pnt * theAxis);

gp_Pnt * hs_gp_Pnt_Mirrored(gp_Pnt * thePnt, gp_Pnt * theAxis);

void hs_gp_Pnt_Mirror1(gp_Pnt * thePnt, gp_Ax1 * theAxis);

gp_Pnt * hs_gp_Pnt_Mirrored1(gp_Pnt * thePnt, gp_Ax1 * theAxis);

void hs_gp_Pnt_Mirror2(gp_Pnt * thePnt, gp_Ax1 * theAxis);

gp_Pnt * hs_gp_Pnt_Mirrored2(gp_Pnt * thePnt, gp_Ax1 * theAxis);

void hs_gp_Pnt_Rotate(gp_Pnt * thePnt, gp_Ax1 * theAxis, double amount);

gp_Pnt * hs_gp_Pnt_Rotated(gp_Pnt * thePnt, gp_Ax1 * theAxis, double amount);

void hs_gp_Pnt_Scale(gp_Pnt * thePnt, gp_Pnt * origin, double amount);

gp_Pnt * hs_gp_Pnt_Scaled(gp_Pnt * thePnt, gp_Pnt * origin, double amount);

void hs_gp_Pnt_Transform(gp_Pnt * thePnt, gp_Trsf * trsf);

gp_Pnt * hs_gp_Pnt_Transformed(gp_Pnt * thePnt, gp_Trsf * trsf);

void hs_gp_Pnt_Translate(gp_Pnt * thePnt, gp_Vec * vec);

gp_Pnt * hs_gp_Pnt_Translated(gp_Pnt * thePnt, gp_Vec * vec);

void hs_gp_Pnt_TranslateRelative(gp_Pnt * thePnt, gp_Pnt * from, gp_Pnt * to);

gp_Pnt * hs_gp_Pnt_TranslatedRelative(gp_Pnt * thePnt, gp_Pnt * from, gp_Pnt * to);

#ifdef __cplusplus
}
#endif

#endif // HS_GP_PNT_H
