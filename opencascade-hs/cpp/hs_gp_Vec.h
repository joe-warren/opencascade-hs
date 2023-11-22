#ifndef HS_GP_VEC_H
#define HS_GP_VEC_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

gp_Vec * hs_new_gp_Vec(double x, double y, double z);

void hs_delete_gp_Vec(gp_Vec* vec);

double hs_gp_Vec_X(gp_Vec * vec);

double hs_gp_Vec_Y(gp_Vec * vec);

double hs_gp_Vec_Z(gp_Vec * vec);

void hs_gp_Vec_SetX(gp_Vec * vec, double x);

void hs_gp_Vec_SetY(gp_Vec * vec, double y);

void hs_gp_Vec_SetZ(gp_Vec * vec, double z);

bool hs_gp_Vec_IsEqual(gp_Vec * a, gp_Vec * b, double linearTolerance, double angularTolerance);

bool hs_gp_Vec_IsNormal(gp_Vec * a, gp_Vec * b, double angularTolerance);

bool hs_gp_Vec_IsOpposite(gp_Vec * a, gp_Vec * b, double angularTolerance);

bool hs_gp_Vec_IsParallel(gp_Vec * a, gp_Vec * b, double angularTolerance);

double hs_gp_Vec_Angle(gp_Vec * a, gp_Vec * b);

double hs_gp_Vec_AngleWithRef(gp_Vec * a, gp_Vec * b, gp_Vec* theVRef);

double hs_gp_Vec_Magnitude(gp_Vec * a);

double hs_gp_Vec_SquareMagnitude(gp_Vec * a);

void hs_gp_Vec_Add(gp_Vec * a, gp_Vec * b);

gp_Vec * hs_gp_Vec_Added(gp_Vec * a, gp_Vec * b);

void hs_gp_Vec_Subtract(gp_Vec * a, gp_Vec * b);

gp_Vec * hs_gp_Vec_Subtracted(gp_Vec * a, gp_Vec * b);

void hs_gp_Vec_Multiply(gp_Vec * a, double b);

gp_Vec * hs_gp_Vec_Multiplied(gp_Vec * a, double b);


void hs_gp_Vec_Divide(gp_Vec * a, double b);

gp_Vec * hs_gp_Vec_Divided(gp_Vec * a, double b);

void hs_gp_Vec_Cross(gp_Vec * a, gp_Vec * b);

gp_Vec * hs_gp_Vec_Crossed(gp_Vec * a, gp_Vec * b);

void hs_gp_Vec_CrossCross(gp_Vec * a, gp_Vec * b, gp_Vec * c);

double hs_gp_Vec_CrossMagnitude(gp_Vec * a, gp_Vec * b);

double hs_gp_Vec_CrossSquareMagnitude(gp_Vec * a, gp_Vec * b);

gp_Vec * hs_gp_Vec_CrossCrossed(gp_Vec * a, gp_Vec * b, gp_Vec * c);

double hs_gp_Vec_Dot(gp_Vec * a, gp_Vec * b);

double hs_gp_Vec_DotCross(gp_Vec * a, gp_Vec * b, gp_Vec * c);

void hs_gp_Vec_Normalize(gp_Vec * a);

gp_Vec * hs_gp_Vec_Normalized(gp_Vec * a);

void hs_gp_Vec_Reverse(gp_Vec* a);

gp_Vec * hs_gp_Vec_Reversed(gp_Vec* a);

void hs_gp_Vec_Mirror(gp_Vec * theVec, gp_Vec * theAxis);

gp_Vec * hs_gp_Vec_Mirrored(gp_Vec * theVec, gp_Vec * theAxis);

void hs_gp_Vec_MirrorAboutAx1(gp_Vec * theVec, gp_Ax1 * theAxis);

gp_Vec * hs_gp_Vec_MirroredAboutAx1(gp_Vec * theVec, gp_Ax1 * theAxis);

void hs_gp_Vec_MirrorAboutAx2(gp_Vec * theVec, gp_Ax2 * theAxis);

gp_Vec * hs_gp_Vec_MirroredAboutAx2(gp_Vec * theVec, gp_Ax2 * theAxis);

void hs_gp_Vec_Rotate(gp_Vec * theVec, gp_Ax1 * theAxis, double amount);

gp_Vec * hs_gp_Vec_Rotated(gp_Vec * theVec, gp_Ax1 * theAxis, double amount);

void hs_gp_Vec_Scale(gp_Vec * theVec, double s);

gp_Vec * hs_gp_Vec_Scaled(gp_Vec * theVec, double s);

void hs_gp_Vec_Transform(gp_Vec * theVec, gp_Trsf * trsf);

gp_Vec * hs_gp_Vec_Transformed(gp_Vec * theVec, gp_Trsf * trsf);

#ifdef __cplusplus
}
#endif

#endif // HS_GP_VEC_H
