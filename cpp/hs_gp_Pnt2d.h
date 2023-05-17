#ifndef HS_GP_PNT2D_H
#define HS_GP_PNT2D_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

gp_Pnt2d * hs_new_gp_Pnt2d(double x, double y);

void hs_delete_gp_Pnt2d(gp_Pnt2d* pnt);

double hs_gp_Pnt2d_X(gp_Pnt2d * pnt);

double hs_gp_Pnt2d_Y(gp_Pnt2d * pnt);

void hs_gp_Pnt2d_SetX(gp_Pnt2d * pnt, double x);

void hs_gp_Pnt2d_SetY(gp_Pnt2d * pnt, double y);

double hs_gp_Pnt2d_Distance(gp_Pnt2d * a, gp_Pnt2d * b );

double hs_gp_Pnt2d_SquareDistance(gp_Pnt2d * a, gp_Pnt2d * b );

bool hs_gp_Pnt2d_IsEqual(gp_Pnt2d * a, gp_Pnt2d * b, double tolerance);

void hs_gp_Pnt2d_Mirror(gp_Pnt2d * thePnt2d, gp_Pnt2d * theAxis);

gp_Pnt2d * hs_gp_Pnt2d_Mirrored(gp_Pnt2d * thePnt2d, gp_Pnt2d * theAxis);

void hs_gp_Pnt2d_MirrorAboutAx2d(gp_Pnt2d * thePnt2d, gp_Ax2d * theAxis);

gp_Pnt2d * hs_gp_Pnt2d_MirroredAboutAx2d(gp_Pnt2d * thePnt2d, gp_Ax2d * theAxis);

void hs_gp_Pnt2d_Rotate(gp_Pnt2d * thePnt2d, gp_Pnt2d * theAxis, double amount);

gp_Pnt2d * hs_gp_Pnt2d_Rotated(gp_Pnt2d * thePnt2d, gp_Pnt2d * theAxis, double amount);

void hs_gp_Pnt2d_Scale(gp_Pnt2d * thePnt2d, gp_Pnt2d * origin, double amount);

gp_Pnt2d * hs_gp_Pnt2d_Scaled(gp_Pnt2d * thePnt2d, gp_Pnt2d * origin, double amount);

void hs_gp_Pnt2d_Transform(gp_Pnt2d * thePnt2d, gp_Trsf2d * trsf);

gp_Pnt2d * hs_gp_Pnt2d_Transformed(gp_Pnt2d * thePnt2d, gp_Trsf2d * trsf);

void hs_gp_Pnt2d_Translate(gp_Pnt2d * thePnt2d, gp_Vec2d * vec);

gp_Pnt2d * hs_gp_Pnt2d_Translated(gp_Pnt2d * thePnt2d, gp_Vec2d * vec);

void hs_gp_Pnt2d_TranslateRelative(gp_Pnt2d * thePnt2d, gp_Pnt2d * from, gp_Pnt2d * to);

gp_Pnt2d * hs_gp_Pnt2d_TranslatedRelative(gp_Pnt2d * thePnt2d, gp_Pnt2d * from, gp_Pnt2d * to);

#ifdef __cplusplus
}
#endif

#endif // HS_GP_DIR_H
