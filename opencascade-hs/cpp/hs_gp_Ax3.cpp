#include <gp_Ax3.hxx>
#include "hs_gp_Ax3.h"


gp_Ax3 * hs_new_gp_Ax3(){
    return new gp_Ax3();
}

gp_Ax3 * hs_new_gp_Ax3_fromAx2(gp_Ax2 * ax){
    return new gp_Ax3(*ax);
}


gp_Ax3 * hs_new_gp_Ax3_fromPntDirAndDir(gp_Pnt * pnt, gp_Dir *n, gp_Dir *v){
    return new gp_Ax3(*pnt, *n, *v);
}

gp_Ax3 * hs_new_gp_Ax3_fromPntAndDir(gp_Pnt * pnt, gp_Dir *n){
    return new gp_Ax3(*pnt, *n);
}

void hs_delete_gp_Ax3(gp_Ax3* axis){
    delete axis;
}

void hs_gp_Ax3_xReverse(gp_Ax3 * axis){
    axis->XReverse();
}

void hs_gp_Ax3_yReverse(gp_Ax3 * axis){
    axis->YReverse();
}

void hs_gp_Ax3_zReverse(gp_Ax3 * axis){
    axis->ZReverse();
}

void hs_gp_Ax3_setAxis(gp_Ax3 * axis, gp_Ax1 * mainAxis){
    axis->SetAxis(*mainAxis);
}

void hs_gp_Ax3_setDirection(gp_Ax3 * axis, gp_Dir * dir){
    axis->SetDirection(*dir);
}

void hs_gp_Ax3_setLocation(gp_Ax3 * axis, gp_Pnt * loc){
    axis->SetLocation(*loc);
}

void hs_gp_Ax3_setXDirection(gp_Ax3 * axis, gp_Dir * dir){
    axis->SetXDirection(*dir);
}

void hs_gp_Ax3_setYDirection(gp_Ax3 * axis, gp_Dir * dir){
    axis->SetYDirection(*dir);
}

double hs_gp_Ax3_angle(gp_Ax3 *axis, gp_Ax3 * other){
    return axis->Angle(*other);
}

gp_Ax1 * hs_gp_Ax3_axis(gp_Ax3 *axis){
    return new gp_Ax1(axis->Axis());
}

gp_Ax2 * hs_gp_Ax3_ax2(gp_Ax3 * axis){
    return new gp_Ax2(axis->Ax2());
}

gp_Dir * hs_gp_Ax3_direction(gp_Ax3 * axis){
    return new gp_Dir(axis->Direction());
}

gp_Pnt * hs_gp_Ax3_location(gp_Ax3 *axis){
    return new gp_Pnt(axis->Location());
}

gp_Dir * hs_gp_Ax3_xDirection(gp_Ax3 * axis){
    return new gp_Dir(axis->XDirection());
}

gp_Dir * hs_gp_Ax3_yDirection(gp_Ax3 * axis){
    return new gp_Dir(axis->YDirection());
}

bool hs_gp_Ax3_direct(gp_Ax3 * axis){
    return axis->Direct();
}

bool hs_gp_Ax3_isCoplanar(gp_Ax3 * axis, gp_Ax3 * other, double linearTolerance, double angularTolerance){
    return axis->IsCoplanar(*other, linearTolerance, angularTolerance);
}

bool hs_gp_Ax3_isCoplanarAx1(gp_Ax3 * axis, gp_Ax1 * other, double linearTolerance, double angularTolerance){
    return axis->IsCoplanar(*other, linearTolerance, angularTolerance);
}

void hs_gp_Ax3_mirror(gp_Ax3 * axis, gp_Ax1 *mirrorAxis){
    axis->Mirror(*mirrorAxis);
}

gp_Ax3* hs_gp_Ax3_mirrored(gp_Ax3 * axis, gp_Ax1 * mirrorAxis){
    return new gp_Ax3(axis->Mirrored(*mirrorAxis));
}

void hs_gp_Ax3_mirror_Ax2(gp_Ax3 * axis, gp_Ax2 *mirrorAxis){
    axis->Mirror(*mirrorAxis);
}

gp_Ax3* hs_gp_Ax3_mirrored_Ax2(gp_Ax3 * axis, gp_Ax2 * mirrorAxis){
    return new gp_Ax3(axis->Mirrored(*mirrorAxis));
}

void hs_gp_Ax3_rotate(gp_Ax3 * axis, gp_Ax1 *rotAxis, double angle){
    axis->Rotate(*rotAxis, angle);
}

gp_Ax3* hs_gp_Ax3_rotated(gp_Ax3 * axis, gp_Ax1 * rotAxis, double angle){
    return new gp_Ax3(axis->Rotated(*rotAxis, angle));
}

void hs_gp_Ax3_scale(gp_Ax3 * axis, gp_Pnt *center, double factor){
    axis->Scale(*center, factor);
}

gp_Ax3* hs_gp_Ax3_scaled(gp_Ax3 * axis, gp_Pnt * center, double factor){
    return new gp_Ax3(axis->Scaled(*center, factor));
}

void hs_gp_Ax3_transform(gp_Ax3 * axis, gp_Trsf *trsf){
    axis->Transform(*trsf);
}

gp_Ax3* hs_gp_Ax3_transformed(gp_Ax3 * axis, gp_Trsf * trsf){
    return new gp_Ax3(axis->Transformed(*trsf));
}

void hs_gp_Ax3_translate(gp_Ax3 * axis, gp_Vec *vec){
    axis->Translate(*vec);
}

gp_Ax3* hs_gp_Ax3_translated(gp_Ax3 * axis, gp_Vec * vec){
    return new gp_Ax3(axis->Translated(*vec));
}


void hs_gp_Ax3_translateRelative(gp_Ax3 * axis, gp_Pnt *from, gp_Pnt *to){
    axis->Translate(*from, *to);
}

gp_Ax3* hs_gp_Ax3_translatedRelative(gp_Ax3 * axis, gp_Pnt * from, gp_Pnt *to){
    return new gp_Ax3(axis->Translated(*from, *to));
}







