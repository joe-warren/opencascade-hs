#ifndef HS_GP_PLN_H
#define HS_GP_PLN_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

gp_Pln * hs_new_gp_Pln();

void hs_delete_gp_Pln(gp_Pln * pln) ;

gp_Pln * hs_new_gp_Pln_fromAx3(gp_Ax3 * ax);

gp_Pln * hs_new_gp_Pln_fromPntAndDir(gp_Pnt * pnt, gp_Dir * dir);

gp_Pln * hs_new_gp_Pln_fromCartesianEquation(double theA, double theB, double theC, double theD, HSExceptionType* exType, void** exPtr);

void hs_gp_Pln_SetAxis(gp_Pln * pln, gp_Ax1 *ax, HSExceptionType* exType, void** exPtr);

void hs_gp_Pln_SetLocation(gp_Pln * pln, gp_Pnt * loc);

void hs_gp_Pln_SetPosition(gp_Pln * pln, gp_Ax3 * pos);

void hs_gp_Pln_VReverse(gp_Pln *pln);

void hs_gp_Pln_UReverse(gp_Pln *pln);

bool hs_gp_Pln_Direct(gp_Pln *pln);

gp_Ax1 * hs_gp_Pln_Axis (gp_Pln *pln);

gp_Pnt * hs_gp_Pln_Location(gp_Pln * pln);

gp_Ax3 * hs_gp_Pln_Position(gp_Pln * pln);

double hs_gp_Pln_DistancePnt(gp_Pln * pln, gp_Pnt* pnt) ;

double hs_gp_Pln_DistanceLin(gp_Pln * pln, gp_Lin* lin) ;

double hs_gp_Pln_DistancePln(gp_Pln * pln, gp_Pln* other) ;

double hs_gp_Pln_SquareDistancePnt(gp_Pln * pln, gp_Pnt* pnt) ;

double hs_gp_Pln_SquareDistanceLin(gp_Pln * pln, gp_Lin* lin) ;

double hs_gp_Pln_SquareDistancePln(gp_Pln * pln, gp_Pln* other) ;

gp_Ax1 * hs_gp_Pln_XAxis(gp_Pln * pln) ;

gp_Ax1 * hs_gp_Pln_YAxis(gp_Pln * pln) ;

bool hs_gp_Pln_ContainsPnt(gp_Pln * pln, gp_Pnt* pnt, double tolerance);

bool hs_gp_Pln_ContainsLin(gp_Pln * pln, gp_Lin* lin, double linearTolerance, double angularTolerance);

void hs_gp_Pln_Coefficients(gp_Pln * pln, double * a, double * b, double * c, double * d);

void hs_gp_Pln_Mirror(gp_Pln * pln, gp_Pnt * pnt);

gp_Pln * hs_gp_Pln_Mirrored(gp_Pln * pln, gp_Pnt * pnt);

void hs_gp_Pln_MirrorAboutAx1(gp_Pln * pln, gp_Ax1 * axis);

gp_Pln * hs_gp_Pln_MirroredAboutAx1(gp_Pln * pln, gp_Ax1 * axis);

void hs_gp_Pln_MirrorAboutAx2(gp_Pln * pln, gp_Ax2 * axis);

gp_Pln * hs_gp_Pln_MirroredAboutAx2(gp_Pln * pln, gp_Ax2 * axis);

void hs_gp_Pln_Rotate(gp_Pln * pln, gp_Ax1 * axis, double amount);

gp_Pln * hs_gp_Pln_Rotated(gp_Pln * pln, gp_Ax1 * axis, double amount);

void hs_gp_Pln_Scale(gp_Pln * pln, gp_Pnt * origin, double amount);

gp_Pln * hs_gp_Pln_Scaled(gp_Pln * pln, gp_Pnt * origin, double amount);

void hs_gp_Pln_Transform(gp_Pln * pln, gp_Trsf * trsf);

gp_Pln * hs_gp_Pln_Transformed(gp_Pln * pln, gp_Trsf * trsf);

void hs_gp_Pln_Translate(gp_Pln * pln, gp_Vec * vec);

gp_Pln * hs_gp_Pln_Translated(gp_Pln * pln, gp_Vec * vec);

void hs_gp_Pln_TranslateRelative(gp_Pln * pln, gp_Pnt * from, gp_Pnt * to);

gp_Pln * hs_gp_Pln_TranslatedRelative(gp_Pln * pln, gp_Pnt * from, gp_Pnt * to);

#ifdef __cplusplus
}
#endif

#endif // HS_GP_PLN_H
