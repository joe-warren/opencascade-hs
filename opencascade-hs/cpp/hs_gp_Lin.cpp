#include <gp_Lin.hxx>
#include "hs_Exception.h"
#include "hs_gp_Lin.h"

gp_Lin * hs_new_gp_Lin(gp_Pnt * origin, gp_Dir * direction){
    return new gp_Lin(*origin, *direction);
}

gp_Lin * hs_new_gp_Lin_fromAx1(gp_Ax1 * axis){
    return new gp_Lin(*axis);
}

void hs_delete_gp_Lin(gp_Lin* lin){
    delete lin;
}

gp_Pnt * hs_gp_Lin_Location(gp_Lin* lin){
    return new gp_Pnt(lin->Location());
}

gp_Dir * hs_gp_Lin_Direction(gp_Lin* lin){
    return new gp_Dir(lin->Direction());
}

gp_Ax1 * hs_gp_Lin_Position(gp_Lin* lin){
    return new gp_Ax1(lin->Position());
}

void hs_gp_Lin_SetLocation(gp_Lin* lin, gp_Pnt * pnt){
    lin->SetLocation(*pnt);
}

void hs_gp_Lin_SetDirection(gp_Lin* lin, gp_Dir * dir){
    lin->SetDirection(*dir);
}

void hs_gp_Lin_SetPosition(gp_Lin* lin, gp_Ax1 * axis){
    lin->SetPosition(*axis);
}

void hs_gp_Lin_Reverse(gp_Lin* lin){
    lin->Reverse();
}

gp_Lin * hs_gp_Lin_Reversed(gp_Lin* lin){
    return new gp_Lin(lin->Reversed());
}

double hs_gp_Lin_Angle(gp_Lin* lin, gp_Lin* other){
    return lin->Angle(*other);
}

bool hs_gp_Lin_Contains(gp_Lin* lin, gp_Pnt* pnt, double linearTolerance){
    return lin->Contains(*pnt, linearTolerance);
}

double hs_gp_Lin_DistancePnt(gp_Lin* lin, gp_Pnt* pnt){
    return lin->Distance(*pnt);
}

double hs_gp_Lin_DistanceLin(gp_Lin* lin, gp_Lin* other){
    return lin->Distance(*other);
}

double hs_gp_Lin_SquareDistancePnt(gp_Lin* lin, gp_Pnt* pnt){
    return lin->SquareDistance(*pnt);
}

double hs_gp_Lin_SquareDistanceLin(gp_Lin* lin, gp_Lin* other){
    return lin->SquareDistance(*other);
}

gp_Lin * hs_gp_Lin_Normal(gp_Lin* lin, gp_Pnt* pnt, HSExceptionType* exType, void** exPtr){
    return hs_handleEx(exType, exPtr, [lin, pnt]{
        return new gp_Lin(lin->Normal(*pnt));
    });
}

void hs_gp_Lin_Mirror(gp_Lin * lin, gp_Pnt * pnt){
    lin->Mirror(*pnt);
}

gp_Lin * hs_gp_Lin_Mirrored(gp_Lin * lin, gp_Pnt * pnt){
    return new gp_Lin(lin->Mirrored(*pnt));
}

void hs_gp_Lin_MirrorAboutAx1(gp_Lin * lin, gp_Ax1 * axis){
    lin->Mirror(*axis);
}

gp_Lin * hs_gp_Lin_MirroredAboutAx1(gp_Lin * lin, gp_Ax1 * axis){
    return new gp_Lin(lin->Mirrored(*axis));
}

void hs_gp_Lin_MirrorAboutAx2(gp_Lin * lin, gp_Ax2 * axis){
    lin->Mirror(*axis);
}

gp_Lin * hs_gp_Lin_MirroredAboutAx2(gp_Lin * lin, gp_Ax2 * axis){
    return new gp_Lin(lin->Mirrored(*axis));
}

void hs_gp_Lin_Rotate(gp_Lin * lin, gp_Ax1 * axis, double amount){
    lin->Rotate(*axis, amount);
}

gp_Lin * hs_gp_Lin_Rotated(gp_Lin * lin, gp_Ax1 * axis, double amount){
    return new gp_Lin(lin->Rotated(*axis, amount));
}

void hs_gp_Lin_Scale(gp_Lin * lin, gp_Pnt * origin, double amount){
    lin->Scale(*origin, amount);
}

gp_Lin * hs_gp_Lin_Scaled(gp_Lin * lin, gp_Pnt * origin, double amount){
    return new gp_Lin(lin->Scaled(*origin, amount));
}

void hs_gp_Lin_Transform(gp_Lin * lin, gp_Trsf * trsf){
    lin->Transform(*trsf);
}

gp_Lin * hs_gp_Lin_Transformed(gp_Lin * lin, gp_Trsf * trsf){
    return new gp_Lin(lin->Transformed(*trsf));
}

void hs_gp_Lin_Translate(gp_Lin * lin, gp_Vec * vec){
    lin->Translate(*vec);
}

gp_Lin * hs_gp_Lin_Translated(gp_Lin * lin, gp_Vec * vec){
    return new gp_Lin(lin->Translated(*vec));
}

void hs_gp_Lin_TranslateRelative(gp_Lin * lin, gp_Pnt * from, gp_Pnt * to){
    lin->Translate(*from, *to);
}

gp_Lin * hs_gp_Lin_TranslatedRelative(gp_Lin * lin, gp_Pnt * from, gp_Pnt * to){
    return new gp_Lin(lin->Translated(*from, *to));
}
