#ifndef HS_GP_AX3_H
#define HS_GP_AX3_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

gp_Ax3 * hs_new_gp_Ax3();

gp_Ax3 * hs_new_gp_Ax3_fromAx2(gp_Ax2 * ax);

gp_Ax3 * hs_new_gp_Ax3_fromPntDirAndDir(gp_Pnt * pnt, gp_Dir *n, gp_Dir *v);

gp_Ax3 * hs_new_gp_Ax3_fromPntAndDir(gp_Pnt * pnt, gp_Dir *n);

void hs_delete_gp_Ax3(gp_Ax3* axis);

void hs_gp_Ax3_xReverse(gp_Ax3 * axis);

void hs_gp_Ax3_yReverse(gp_Ax3 * axis);

void hs_gp_Ax3_zReverse(gp_Ax3 * axis);

void hs_gp_Ax3_setAxis(gp_Ax3 * axis, gp_Ax1 * mainAxis);

void hs_gp_Ax3_setDirection(gp_Ax3 * axis, gp_Dir * dir);

void hs_gp_Ax3_setLocation(gp_Ax3 * axis, gp_Pnt * loc);

void hs_gp_Ax3_setXDirection(gp_Ax3 * axis, gp_Dir * dir);

void hs_gp_Ax3_setYDirection(gp_Ax3 * axis, gp_Dir * dir);

double hs_gp_Ax3_angle(gp_Ax3 *axis, gp_Ax3 *other);

gp_Ax1 * hs_gp_Ax3_axis(gp_Ax3 *axis);

gp_Ax2 * hs_gp_Ax3_ax2(gp_Ax3 * axis);

gp_Dir * hs_gp_Ax3_direction(gp_Ax3 * axis);

gp_Pnt * hs_gp_Ax3_location(gp_Ax3 *axis);

gp_Dir * hs_gp_Ax3_xDirection(gp_Ax3 * axis);

gp_Dir * hs_gp_Ax3_yDirection(gp_Ax3 * axis);

bool hs_gp_Ax3_direct(gp_Ax3 * axis);

bool hs_gp_Ax3_isCoplanar(gp_Ax3 * axis, gp_Ax3 * other, double linearTolerance, double angularTolerance);

bool hs_gp_Ax3_isCoplanarAx1(gp_Ax3 * axis, gp_Ax1 * other, double linearTolerance, double angularTolerance);

void hs_gp_Ax3_mirror(gp_Ax3 * axis, gp_Ax1 *mirrorAxis);

gp_Ax3* hs_gp_Ax3_mirrored(gp_Ax3 * axis, gp_Ax1 * mirrorAxis);

void hs_gp_Ax3_mirror_Ax2(gp_Ax3 * axis, gp_Ax2 *mirrorAxis);

gp_Ax3* hs_gp_Ax3_mirrored_Ax2(gp_Ax3 * axis, gp_Ax2 * mirrorAxis);

void hs_gp_Ax3_rotate(gp_Ax3 * axis, gp_Ax1 *rotAxis, double angle);

gp_Ax3* hs_gp_Ax3_rotated(gp_Ax3 * axis, gp_Ax1 * rotAxis, double angle);

void hs_gp_Ax3_scale(gp_Ax3 * axis, gp_Pnt *center, double factor);

gp_Ax3* hs_gp_Ax3_scaled(gp_Ax3 * axis, gp_Pnt * center, double factor);

void hs_gp_Ax3_transform(gp_Ax3 * axis, gp_Trsf *trsf);

gp_Ax3* hs_gp_Ax3_transformed(gp_Ax3 * axis, gp_Trsf * trsf);

void hs_gp_Ax3_translate(gp_Ax3 * axis, gp_Vec *vec);

gp_Ax3* hs_gp_Ax3_translated(gp_Ax3 * axis, gp_Vec * vec);

void hs_gp_Ax3_translateRelative(gp_Ax3 * axis, gp_Pnt *from, gp_Pnt *to);

gp_Ax3* hs_gp_Ax3_translatedRelative(gp_Ax3 * axis, gp_Pnt * from, gp_Pnt *to);


#ifdef __cplusplus
}
#endif

#endif // HS_GP_AX3_H
