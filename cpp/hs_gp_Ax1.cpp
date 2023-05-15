#include <gp_Ax1.hxx>
#include "hs_gp_Ax1.h"

gp_Ax1 * hs_new_gp_Ax1(gp_Pnt * origin, gp_Dir * direction){
    return new gp_Ax1(*origin, *direction);
}

void hs_delete_gp_Ax1(gp_Ax1* ax1){
    delete ax1;
}

gp_Dir * hs_gp_Ax1_Direction(gp_Ax1* ax1){
    return new gp_Dir(ax1->Direction());
}

gp_Pnt * hs_gp_Ax1_Location(gp_Ax1* ax1){
    return new gp_Pnt(ax1->Location());
}

void hs_gp_Ax1_SetDirection(gp_Ax1* ax1, gp_Dir* direction){
    ax1->SetDirection(*direction);
}

void hs_gp_Ax1_SetLocation(gp_Ax1* ax1, gp_Pnt* origin){
    ax1->SetLocation(*origin);
}

bool hs_gp_Ax1_IsCoaxial(gp_Ax1* axis1, gp_Ax1* axis2, double angularTolerance, double linearTolerance){
    return axis1->IsCoaxial(*axis2, angularTolerance, linearTolerance);
}

bool hs_gp_Ax1_IsNormal(gp_Ax1* axis1, gp_Ax1* axis2, double angularTolerance){
    return axis1->IsNormal(*axis2, angularTolerance);
}

bool hs_gp_Ax1_IsOpposite(gp_Ax1* axis1, gp_Ax1* axis2, double angularTolerance){
    return axis1->IsOpposite(*axis2, angularTolerance);
}

bool hs_gp_Ax1_IsParallel(gp_Ax1* axis1, gp_Ax1* axis2, double angularTolerance){
    return axis1->IsParallel(*axis2, angularTolerance);
}

double hs_gp_Ax1_Angle(gp_Ax1* axis1, gp_Ax1* axis2){
    return axis1->Angle(*axis2);
}

void hs_gp_Ax1_Reverse(gp_Ax1* ax1){
    ax1->Reverse();
}

gp_Ax1 * hs_gp_Ax1_Reversed(gp_Ax1* ax1){
    return new gp_Ax1(ax1->Reversed());
}

void hs_gp_Ax1_Mirror(gp_Ax1* ax1, gp_Ax1* mirror){
    ax1->Mirror(*mirror);
}

gp_Ax1 * hs_gp_Ax1_Mirrored(gp_Ax1* ax1, gp_Ax1* mirror){
    return new gp_Ax1(ax1->Mirrored(*mirror));
}

void hs_gp_Ax1_MirrorAboutPnt(gp_Ax1* ax1, gp_Pnt* mirror){
    ax1->Mirror(*mirror);
}

gp_Ax1 * hs_gp_Ax1_MirroredAboutPnt(gp_Ax1* ax1, gp_Pnt* mirror){
    return new gp_Ax1(ax1->Mirrored(*mirror));
}

void hs_gp_Ax1_MirrorAboutAx2(gp_Ax1* ax1, gp_Ax2* mirror){
    ax1->Mirror(*mirror);
}

gp_Ax1 * hs_gp_Ax1_MirroredAboutAx2(gp_Ax1* ax1, gp_Ax2* mirror){
    return new gp_Ax1(ax1->Mirrored(*mirror));
}

void hs_gp_Ax1_Rotate(gp_Ax1* ax1, gp_Ax1* axisOfRotation, double angle){
    ax1->Rotate(*axisOfRotation, angle);
}

gp_Ax1 * hs_gp_Ax1_Rotated(gp_Ax1* ax1, gp_Ax1* axisOfRotation, double angle){
    return new gp_Ax1(ax1->Rotated(*axisOfRotation, angle));
}

void hs_gp_Ax1_Scale(gp_Ax1* ax1, gp_Pnt* origin, double amount){
    ax1->Scale(*origin, amount);
}

gp_Ax1 * hs_gp_Ax1_Scaled(gp_Ax1* ax1, gp_Pnt* origin, double amount){
    return new gp_Ax1(ax1->Scaled(*origin, amount));
}

void hs_gp_Ax1_Transform(gp_Ax1* ax1, gp_Trsf* trsf){
    ax1->Transform(*trsf);
}

gp_Ax1 * hs_gp_Ax1_Transformed(gp_Ax1* ax1, gp_Trsf* trsf){
    return new gp_Ax1(ax1->Transformed(*trsf));
}

void hs_gp_Ax1_Translate(gp_Ax1* ax1, gp_Vec* vec){
    ax1->Translate(*vec);
}

gp_Ax1 * hs_gp_Ax1_Translated(gp_Ax1* ax1, gp_Vec* vec){
    return new gp_Ax1(ax1->Translated(*vec));
}

void hs_gp_Ax1_TranslateRelative(gp_Ax1* ax1, gp_Pnt* from, gp_Pnt* to){
    ax1->Translate(*from, *to);
}

gp_Ax1 * hs_gp_Ax1_TranslatedRelative(gp_Ax1* ax1, gp_Pnt* from, gp_Pnt* to){
    return new gp_Ax1(ax1->Translated(*from, *to));
}
