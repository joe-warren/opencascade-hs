#include <gp_Ax2d.hxx>
#include "hs_gp_Ax2d.h"

gp_Ax2d* hs_new_gp_Ax2d(gp_Pnt2d* location, gp_Dir2d* direction){
    return new gp_Ax2d(*location, *direction);
}

void hs_delete_gp_Ax2d(gp_Ax2d* ax2d){
    delete ax2d;
}

gp_Dir2d * hs_gp_Ax2d_Direction(gp_Ax2d* ax2d){
    return new gp_Dir2d(ax2d->Direction());
}

gp_Pnt2d * hs_gp_Ax2d_Location(gp_Ax2d* ax2d){
    return new gp_Pnt2d(ax2d->Location());
}

void hs_gp_Ax2d_SetDirection(gp_Ax2d* ax2d, gp_Dir2d* direction){
    ax2d->SetDirection(*direction);
}

void hs_gp_Ax2d_SetLocation(gp_Ax2d* ax2d, gp_Pnt2d* origin){
    ax2d->SetLocation(*origin);
}

bool hs_gp_Ax2d_IsCoaxial(gp_Ax2d* axis1, gp_Ax2d* axis2, double angularTolerance, double linearTolerance){
    return axis1->IsCoaxial(*axis2, angularTolerance, linearTolerance);
}

bool hs_gp_Ax2d_IsNormal(gp_Ax2d* axis1, gp_Ax2d* axis2, double angularTolerance){
    return axis1->IsNormal(*axis2, angularTolerance);
}

bool hs_gp_Ax2d_IsOpposite(gp_Ax2d* axis1, gp_Ax2d* axis2, double angularTolerance){
    return axis1->IsOpposite(*axis2, angularTolerance);
}

bool hs_gp_Ax2d_IsParallel(gp_Ax2d* axis1, gp_Ax2d* axis2, double angularTolerance){
    return axis1->IsParallel(*axis2, angularTolerance);
}

double hs_gp_Ax2d_Angle(gp_Ax2d* axis1, gp_Ax2d* axis2){
    return axis1->Angle(*axis2);
}

void hs_gp_Ax2d_Reverse(gp_Ax2d* ax2d){
    ax2d->Reverse();
}

gp_Ax2d * hs_gp_Ax2d_Reversed(gp_Ax2d* ax2d){
    return new gp_Ax2d(ax2d->Reversed());
}

void hs_gp_Ax2d_Mirror(gp_Ax2d* ax2d, gp_Ax2d* mirror){
    ax2d->Mirror(*mirror);
}

gp_Ax2d * hs_gp_Ax2d_Mirrored(gp_Ax2d* ax2d, gp_Ax2d* mirror){
    return new gp_Ax2d(ax2d->Mirrored(*mirror));
}

void hs_gp_Ax2d_MirrorAboutPnt2d(gp_Ax2d* ax2d, gp_Pnt2d* mirror){
    ax2d->Mirror(*mirror);
}

gp_Ax2d * hs_gp_Ax2d_MirroredAboutPnt2d(gp_Ax2d* ax2d, gp_Pnt2d* mirror){
    return new gp_Ax2d(ax2d->Mirrored(*mirror));
}

void hs_gp_Ax2d_Rotate(gp_Ax2d* ax2d, gp_Pnt2d* axisOfRotation, double angle){
    ax2d->Rotate(*axisOfRotation, angle);
}

gp_Ax2d * hs_gp_Ax2d_Rotated(gp_Ax2d* ax2d, gp_Pnt2d* axisOfRotation, double angle){
    return new gp_Ax2d(ax2d->Rotated(*axisOfRotation, angle));
}

void hs_gp_Ax2d_Scale(gp_Ax2d* ax2d, gp_Pnt2d* origin, double amount){
    ax2d->Scale(*origin, amount);
}

gp_Ax2d * hs_gp_Ax2d_Scaled(gp_Ax2d* ax2d, gp_Pnt2d* origin, double amount){
    return new gp_Ax2d(ax2d->Scaled(*origin, amount));
}

void hs_gp_Ax2d_Transform(gp_Ax2d* ax2d, gp_Trsf2d* trsf){
    ax2d->Transform(*trsf);
}

gp_Ax2d * hs_gp_Ax2d_Transformed(gp_Ax2d* ax2d, gp_Trsf2d* trsf){
    return new gp_Ax2d(ax2d->Transformed(*trsf));
}

void hs_gp_Ax2d_Translate(gp_Ax2d* ax2d, gp_Vec2d* vec){
    ax2d->Translate(*vec);
}

gp_Ax2d * hs_gp_Ax2d_Translated(gp_Ax2d* ax2d, gp_Vec2d* vec){
    return new gp_Ax2d(ax2d->Translated(*vec));
}

void hs_gp_Ax2d_TranslateRelative(gp_Ax2d* ax2d, gp_Pnt2d* from, gp_Pnt2d* to){
    ax2d->Translate(*from, *to);
}

gp_Ax2d * hs_gp_Ax2d_TranslatedRelative(gp_Ax2d* ax2d, gp_Pnt2d* from, gp_Pnt2d* to){
    return new gp_Ax2d(ax2d->Translated(*from, *to));
}
