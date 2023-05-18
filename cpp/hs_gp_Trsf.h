#ifndef HS_GP_TRSF_H
#define HS_GP_TRSF_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif


gp_Trsf * hs_new_gp_Trsf();

void hs_delete_gp_Trsf(gp_Trsf * t);

gp_Trsf * hs_new_gp_Trsf_fromTrsf2d(gp_Trsf2d * t);

void hs_gp_Trsf_SetMirrorAboutPnt(gp_Trsf * trsf, gp_Pnt * pnt);

void hs_gp_Trsf_SetMirrorAboutAx1(gp_Trsf * trsf, gp_Ax1 * ax);

void hs_gp_Trsf_SetMirrorAboutAx2(gp_Trsf * trsf, gp_Ax2 * ax);

void hs_gp_Trsf_SetRotationAboutAxisAngle(gp_Trsf * trsf, gp_Ax1 * ax, double angle);

void hs_gp_Trsf_SetScale(gp_Trsf * trsf, gp_Pnt * origin, double factor);

void hs_gp_Trsf_SetTranslation(gp_Trsf * trsf, gp_Vec * trans);

void hs_gp_Trsf_SetTranslationPart(gp_Trsf * trsf, gp_Vec * trans);

void hs_gp_Trsf_SetScaleFactor(gp_Trsf * trsf, double s);

void hs_gp_Trsf_SetValues(gp_Trsf * trsf, 
        double a11, double a12, double a13, double a14,
        double a21, double a22, double a23, double a24,
        double a31, double a32, double a33, double a34);

bool hs_gp_Trsf_IsNegative(gp_Trsf * trsf);

double hs_gp_Trsf_ScaleFactor(gp_Trsf * trsf);

double hs_gp_Trsf_Value(gp_Trsf* trsf, int row, int col);

void hs_gp_Trsf_Invert(gp_Trsf* trsf);

gp_Trsf * hs_gp_Trsf_Inverted(gp_Trsf* trsf);

void hs_gp_Trsf_Multiply(gp_Trsf * trsf, gp_Trsf* b);

gp_Trsf * hs_gp_Trsf_Multiplied(gp_Trsf* a, gp_Trsf* b);

void hs_gp_Trsf_PreMultiply(gp_Trsf * trsf, gp_Trsf* b);

void hs_gp_Trsf_Power(gp_Trsf * trsf, int b);

gp_Trsf * hs_gp_Trsf_Powered(gp_Trsf* a, int b);

#ifdef __cplusplus
}
#endif

#endif // HS_GP_TRSF_H
