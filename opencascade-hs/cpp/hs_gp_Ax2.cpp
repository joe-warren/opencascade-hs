#include <gp_Ax2.hxx>
#include "hs_gp_Ax2.h"

gp_Ax2 * hs_new_gp_Ax2(gp_Pnt * origin, gp_Dir * north, gp_Dir * vX){
    return new gp_Ax2(*origin, *north, *vX);
}

void hs_delete_gp_Ax2(gp_Ax2* ax2){
    delete ax2;
}

gp_Ax2 * hs_new_gp_Ax2_autoX(gp_Pnt * origin, gp_Dir * north){
    return new gp_Ax2(*origin, *north);
}

gp_Dir * hs_gp_Ax2_Direction(gp_Ax2* ax2){
    return new gp_Dir(ax2->Direction());
}

gp_Pnt * hs_gp_Ax2_Location(gp_Ax2* ax2){
    return new gp_Pnt(ax2->Location());
}

gp_Dir * hs_gp_Ax2_XDirection(gp_Ax2* ax2){
    return new gp_Dir(ax2->XDirection());
}

gp_Dir * hs_gp_Ax2_YDirection(gp_Ax2* ax2){
    return new gp_Dir(ax2->YDirection());
}

gp_Ax1 * hs_gp_Ax2_Axis(gp_Ax2* ax2){
    return new gp_Ax1(ax2->Axis());
}

void hs_gp_Ax2_SetDirection(gp_Ax2* ax2, gp_Dir* direction){
    ax2->SetDirection(*direction);
}

void hs_gp_Ax2_SetLocation(gp_Ax2* ax2, gp_Pnt* origin){
    ax2->SetLocation(*origin);
}

void hs_gp_Ax2_SetXDirection(gp_Ax2* ax2, gp_Dir* direction){
    ax2->SetXDirection(*direction);
}

void hs_gp_Ax2_SetYDirection(gp_Ax2* ax2, gp_Dir* direction){
    ax2->SetYDirection(*direction);
}

void hs_gp_Ax2_SetAxis(gp_Ax2* ax2, gp_Ax1* ax1){
    ax2->SetAxis(*ax1);
}

bool hs_gp_Ax2_IsCoplanar(gp_Ax2* axis1, gp_Ax2* axis2, double linearTolerance, double angularTolerance){
    return axis1->IsCoplanar(*axis2, linearTolerance, angularTolerance);
}

bool hs_gp_Ax2_IsCoplanarWithAx1(gp_Ax2* axis1, gp_Ax1* axis2, double linearTolerance, double angularTolerance){
    return axis1->IsCoplanar(*axis2, linearTolerance, angularTolerance);
}

void hs_gp_Ax2_Mirror(gp_Ax2* ax2, gp_Ax2* mirror){
    ax2->Mirror(*mirror);
}

gp_Ax2 * hs_gp_Ax2_Mirrored(gp_Ax2* ax2, gp_Ax2* mirror){
    return new gp_Ax2(ax2->Mirrored(*mirror));
}

void hs_gp_Ax2_MirrorAboutPnt(gp_Ax2* ax2, gp_Pnt* mirror){
    ax2->Mirror(*mirror);
}

gp_Ax2 * hs_gp_Ax2_MirroredAboutPnt(gp_Ax2* ax2, gp_Pnt* mirror){
    return new gp_Ax2(ax2->Mirrored(*mirror));
}

void hs_gp_Ax2_MirrorAboutAx1(gp_Ax2* ax2, gp_Ax1* mirror){
    ax2->Mirror(*mirror);
}

gp_Ax2 * hs_gp_Ax2_MirroredAboutAx1(gp_Ax2* ax2, gp_Ax1* mirror){
    return new gp_Ax2(ax2->Mirrored(*mirror));
}

void hs_gp_Ax2_Rotate(gp_Ax2* ax2, gp_Ax1* axisOfRotation, double angle){
    ax2->Rotate(*axisOfRotation, angle);
}

gp_Ax2 * hs_gp_Ax2_Rotated(gp_Ax2* ax2, gp_Ax1* axisOfRotation, double angle){
    return new gp_Ax2(ax2->Rotated(*axisOfRotation, angle));
}

void hs_gp_Ax2_Scale(gp_Ax2* ax2, gp_Pnt* origin, double amount){
    ax2->Scale(*origin, amount);
}

gp_Ax2 * hs_gp_Ax2_Scaled(gp_Ax2* ax2, gp_Pnt* origin, double amount){
    return new gp_Ax2(ax2->Scaled(*origin, amount));
}

void hs_gp_Ax2_Transform(gp_Ax2* ax2, gp_Trsf* trsf){
    ax2->Transform(*trsf);
}

gp_Ax2 * hs_gp_Ax2_Transformed(gp_Ax2* ax2, gp_Trsf* trsf){
    return new gp_Ax2(ax2->Transformed(*trsf));
}

void hs_gp_Ax2_Translate(gp_Ax2* ax2, gp_Vec* vec){
    ax2->Translate(*vec);
}

gp_Ax2 * hs_gp_Ax2_Translated(gp_Ax2* ax2, gp_Vec* vec){
    return new gp_Ax2(ax2->Translated(*vec));
}

void hs_gp_Ax2_TranslateRelative(gp_Ax2* ax2, gp_Pnt* from, gp_Pnt* to){
    ax2->Translate(*from, *to);
}

gp_Ax2 * hs_gp_Ax2_TranslatedRelative(gp_Ax2* ax2, gp_Pnt* from, gp_Pnt* to){
    return new gp_Ax2(ax2->Translated(*from, *to));
}
