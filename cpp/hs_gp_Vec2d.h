#ifndef HS_GP_VEC2D_H
#define HS_GP_VEC2D_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

gp_Vec2d * hs_new_gp_Vec2d(double x, double y);

void hs_delete_gp_Vec2d(gp_Vec2d* vec);

double hs_gp_Vec2d_X(gp_Vec2d * vec);

double hs_gp_Vec2d_Y(gp_Vec2d * vec);

void hs_gp_Vec2d_SetX(gp_Vec2d * vec, double x);

void hs_gp_Vec2d_SetY(gp_Vec2d * vec, double y);

bool hs_gp_Vec2d_IsEqual(gp_Vec2d * a, gp_Vec2d * b, double linearTolerance, double angularTolerance);

bool hs_gp_Vec2d_IsNormal(gp_Vec2d * a, gp_Vec2d * b, double angularTolerance);

bool hs_gp_Vec2d_IsOpposite(gp_Vec2d * a, gp_Vec2d * b, double angularTolerance);

bool hs_gp_Vec2d_IsParallel(gp_Vec2d * a, gp_Vec2d * b, double angularTolerance);

double hs_gp_Vec2d_Angle(gp_Vec2d * a, gp_Vec2d * b);

double hs_gp_Vec2d_Magnitude(gp_Vec2d * a);

double hs_gp_Vec2d_SquareMagnitude(gp_Vec2d * a);

void hs_gp_Vec2d_Add(gp_Vec2d * a, gp_Vec2d * b);

gp_Vec2d * hs_gp_Vec2d_Added(gp_Vec2d * a, gp_Vec2d * b);

void hs_gp_Vec2d_Subtract(gp_Vec2d * a, gp_Vec2d * b);

gp_Vec2d * hs_gp_Vec2d_Subtracted(gp_Vec2d * a, gp_Vec2d * b);

void hs_gp_Vec2d_Multiply(gp_Vec2d * a, double b);

gp_Vec2d * hs_gp_Vec2d_Multiplied(gp_Vec2d * a, double b);


void hs_gp_Vec2d_Divide(gp_Vec2d * a, double b);

gp_Vec2d * hs_gp_Vec2d_Divided(gp_Vec2d * a, double b);

double hs_gp_Vec2d_Crossed(gp_Vec2d * a, gp_Vec2d * b);

double hs_gp_Vec2d_CrossMagnitude(gp_Vec2d * a, gp_Vec2d * b);

double hs_gp_Vec2d_CrossSquareMagnitude(gp_Vec2d * a, gp_Vec2d * b);

double hs_gp_Vec2d_Dot(gp_Vec2d * a, gp_Vec2d * b);

void hs_gp_Vec2d_Normalize(gp_Vec2d * a);

gp_Vec2d * hs_gp_Vec2d_Normalized(gp_Vec2d * a);

void hs_gp_Vec2d_Reverse(gp_Vec2d* a);

gp_Vec2d * hs_gp_Vec2d_Reversed(gp_Vec2d* a);

void hs_gp_Vec2d_Mirror(gp_Vec2d * theVec2d, gp_Vec2d * theAxis);

gp_Vec2d * hs_gp_Vec2d_Mirrored(gp_Vec2d * theVec2d, gp_Vec2d * theAxis);

void hs_gp_Vec2d_MirrorAboutAx2d(gp_Vec2d * theVec2d, gp_Ax2d * theAxis);

gp_Vec2d * hs_gp_Vec2d_MirroredAboutAx2d(gp_Vec2d * theVec2d, gp_Ax2d * theAxis);

void hs_gp_Vec2d_Rotate(gp_Vec2d * theVec2d, double amount);

gp_Vec2d * hs_gp_Vec2d_Rotated(gp_Vec2d * theVec2d, double amount);

void hs_gp_Vec2d_Scale(gp_Vec2d * theVec2d, double s);

gp_Vec2d * hs_gp_Vec2d_Scaled(gp_Vec2d * theVec2d, double s);

void hs_gp_Vec2d_Transform(gp_Vec2d * theVec2d, gp_Trsf2d * trsf);

gp_Vec2d * hs_gp_Vec2d_Transformed(gp_Vec2d * theVec2d, gp_Trsf2d * trsf);

#ifdef __cplusplus
}
#endif

#endif // HS_GP_VEC2D_H
