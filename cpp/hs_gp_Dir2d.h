#ifndef HS_GP_DIR2D_H
#define HS_GP_DIR2D_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

gp_Dir2d * hs_new_gp_Dir2d(double x, double y);

void hs_delete_gp_Dir2d(gp_Dir2d* dir);

double hs_gp_Dir2d_X(gp_Dir2d * pnt);

double hs_gp_Dir2d_Y(gp_Dir2d * pnt);

void hs_gp_Dir2d_SetX(gp_Dir2d * pnt, double x);

void hs_gp_Dir2d_SetY(gp_Dir2d * pnt, double y);

bool hs_gp_Dir2d_IsEqual(gp_Dir2d * a, gp_Dir2d * b, double angularTolerance);

bool hs_gp_Dir2d_IsNormal(gp_Dir2d * a, gp_Dir2d * b, double angularTolerance);

bool hs_gp_Dir2d_IsOpposite(gp_Dir2d * a, gp_Dir2d * b, double angularTolerance);

bool hs_gp_Dir2d_IsParallel(gp_Dir2d * a, gp_Dir2d * b, double angularTolerance);

double hs_gp_Dir2d_Angle(gp_Dir2d * a, gp_Dir2d * b);

double hs_gp_Dir2d_Crossed(gp_Dir2d * a, gp_Dir2d * b);

double hs_gp_Dir2d_Dot(gp_Dir2d * a, gp_Dir2d * b);

void hs_gp_Dir2d_Reverse(gp_Dir2d* ax1);

gp_Dir2d * hs_gp_Dir2d_Reversed(gp_Dir2d* ax1);

void hs_gp_Dir2d_Mirror(gp_Dir2d * theDir2d, gp_Dir2d * theAxis);

gp_Dir2d * hs_gp_Dir2d_Mirrored(gp_Dir2d * theDir2d, gp_Dir2d * theAxis);

void hs_gp_Dir2d_MirrorAboutAx2d(gp_Dir2d * theDir2d, gp_Ax2d * theAxis);

gp_Dir2d * hs_gp_Dir2d_MirroredAboutAx2d(gp_Dir2d * theDir2d, gp_Ax2d * theAxis);

void hs_gp_Dir2d_Rotate(gp_Dir2d * theDir2d, double amount);

gp_Dir2d * hs_gp_Dir2d_Rotated(gp_Dir2d * theDir2d, double amount);

void hs_gp_Dir2d_Transform(gp_Dir2d * theDir2d, gp_Trsf2d * trsf);

gp_Dir2d * hs_gp_Dir2d_Transformed(gp_Dir2d * theDir2d, gp_Trsf2d * trsf);

#ifdef __cplusplus
}
#endif

#endif // HS_GP_DIR_H
