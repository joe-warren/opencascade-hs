#ifndef HS_GP_TRSF2D_H
#define HS_GP_TRSF2D_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

gp_Trsf2d * hs_new_gp_Trsf2d();

void hs_delete_gp_Trsf2d(gp_Trsf2d * t);

gp_Trsf2d * hs_new_gp_Trsf2d_fromTrsf(gp_Trsf * t);

void hs_gp_Trsf2d_SetMirrorAboutPnt2d(gp_Trsf2d * trsf, gp_Pnt2d * pnt);

void hs_gp_Trsf2d_SetMirrorAboutAx2d(gp_Trsf2d * trsf, gp_Ax2d * ax);

void hs_gp_Trsf2d_SetRotation(gp_Trsf2d * trsf, gp_Pnt2d * ax, double angle);

void hs_gp_Trsf2d_SetScale(gp_Trsf2d * trsf, gp_Pnt2d * origin, double factor);

void hs_gp_Trsf2d_SetTransformation(gp_Trsf2d * trsf, gp_Ax2d * to);

void hs_gp_Trsf2d_SetTransformationRelative(gp_Trsf2d * trsf, gp_Ax2d * from, gp_Ax2d * to);

void hs_gp_Trsf2d_SetTranslation(gp_Trsf2d * trsf, gp_Vec2d * trans);

void hs_gp_Trsf2d_SetTranslationRelative(gp_Trsf2d * trsf, gp_Pnt2d * from, gp_Pnt2d * to);

void hs_gp_Trsf2d_SetTranslationPart(gp_Trsf2d * trsf, gp_Vec2d * trans);

void hs_gp_Trsf2d_SetScaleFactor(gp_Trsf2d * trsf, double s);

void hs_gp_Trsf2d_SetValues(gp_Trsf2d * trsf, 
        double a11, double a12, double a13, 
        double a21, double a22, double a23);

bool hs_gp_Trsf2d_IsNegative(gp_Trsf2d * trsf);

double hs_gp_Trsf2d_ScaleFactor(gp_Trsf2d * trsf);

double hs_gp_Trsf2d_Value(gp_Trsf2d* trsf, int row, int col);

void hs_gp_Trsf2d_Invert(gp_Trsf2d* trsf);

gp_Trsf2d * hs_gp_Trsf2d_Inverted(gp_Trsf2d* trsf);

void hs_gp_Trsf2d_Multiply(gp_Trsf2d * trsf, gp_Trsf2d* b);

gp_Trsf2d * hs_gp_Trsf2d_Multiplied(gp_Trsf2d* a, gp_Trsf2d* b);

void hs_gp_Trsf2d_PreMultiply(gp_Trsf2d * trsf, gp_Trsf2d* b);

void hs_gp_Trsf2d_Power(gp_Trsf2d * trsf, int b);

gp_Trsf2d * hs_gp_Trsf2d_Powered(gp_Trsf2d* a, int b);

#ifdef __cplusplus
}
#endif

#endif // HS_GP_TRSF2D_H
