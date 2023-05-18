#ifndef HS_GP_DIR_H
#define HS_GP_DIR_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

gp_Dir * hs_new_gp_Dir(double x, double y, double z);

void hs_delete_gp_Dir(gp_Dir* dir);

double hs_gp_Dir_X(gp_Dir * pnt);

double hs_gp_Dir_Y(gp_Dir * pnt);

double hs_gp_Dir_Z(gp_Dir * pnt);

void hs_gp_Dir_SetX(gp_Dir * pnt, double x);

void hs_gp_Dir_SetY(gp_Dir * pnt, double y);

void hs_gp_Dir_SetZ(gp_Dir * pnt, double z);

bool hs_gp_Dir_IsEqual(gp_Dir * a, gp_Dir * b, double angularTolerance);

bool hs_gp_Dir_IsNormal(gp_Dir * a, gp_Dir * b, double angularTolerance);

bool hs_gp_Dir_IsOpposite(gp_Dir * a, gp_Dir * b, double angularTolerance);

bool hs_gp_Dir_IsParallel(gp_Dir * a, gp_Dir * b, double angularTolerance);

double hs_gp_Dir_Angle(gp_Dir * a, gp_Dir * b);

double hs_gp_Dir_AngleWithRef(gp_Dir * a, gp_Dir * b, gp_Dir* theVRef);

void hs_gp_Dir_Cross(gp_Dir * a, gp_Dir * b);

gp_Dir * hs_gp_Dir_Crossed(gp_Dir * a, gp_Dir * b);

void hs_gp_Dir_CrossCross(gp_Dir * a, gp_Dir * b, gp_Dir * c);

gp_Dir * hs_gp_Dir_CrossCrossed(gp_Dir * a, gp_Dir * b, gp_Dir * c);

double hs_gp_Dir_Dot(gp_Dir * a, gp_Dir * b);

double hs_gp_Dir_DotCross(gp_Dir * a, gp_Dir * b, gp_Dir * c);

void hs_gp_Dir_Reverse(gp_Dir* ax1);

gp_Dir * hs_gp_Dir_Reversed(gp_Dir* ax1);

void hs_gp_Dir_Mirror(gp_Dir * theDir, gp_Dir * theAxis);

gp_Dir * hs_gp_Dir_Mirrored(gp_Dir * theDir, gp_Dir * theAxis);

void hs_gp_Dir_MirrorAboutAx1(gp_Dir * theDir, gp_Ax1 * theAxis);

gp_Dir * hs_gp_Dir_MirroredAboutAx1(gp_Dir * theDir, gp_Ax1 * theAxis);

void hs_gp_Dir_MirrorAboutAx2(gp_Dir * theDir, gp_Ax2 * theAxis);

gp_Dir * hs_gp_Dir_MirroredAboutAx2(gp_Dir * theDir, gp_Ax2 * theAxis);

void hs_gp_Dir_Rotate(gp_Dir * theDir, gp_Ax1 * theAxis, double amount);

gp_Dir * hs_gp_Dir_Rotated(gp_Dir * theDir, gp_Ax1 * theAxis, double amount);

void hs_gp_Dir_Transform(gp_Dir * theDir, gp_Trsf * trsf);

gp_Dir * hs_gp_Dir_Transformed(gp_Dir * theDir, gp_Trsf * trsf);

#ifdef __cplusplus
}
#endif

#endif // HS_GP_DIR_H
