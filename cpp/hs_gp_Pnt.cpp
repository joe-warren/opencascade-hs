#include <gp_Pnt.hxx>
#include "hs_gp_Pnt.h"

gp_Pnt * hs_new_gp_Pnt(double x, double y, double z) {
    return new gp_Pnt(x, y, z);
}

void hs_delete_gp_Pnt(gp_Pnt* pnt){
    delete pnt;
}

double hs_gp_Pnt_X(gp_Pnt * pnt){
    return pnt->X();
}

double hs_gp_Pnt_Y(gp_Pnt * pnt){
    return pnt->Y();
}

double hs_gp_Pnt_Z(gp_Pnt * pnt){
    return pnt->Z();
}

void hs_gp_Pnt_SetX(gp_Pnt * pnt, double x){
    pnt->SetX(x);
}

void hs_gp_Pnt_SetY(gp_Pnt * pnt, double y){
    pnt->SetY(y);
}

void hs_gp_Pnt_SetZ(gp_Pnt * pnt, double z){
    pnt->SetZ(z);
}

double hs_gp_Pnt_Distance(gp_Pnt * a, gp_Pnt * b ){
    return a->Distance(*b);
}

double hs_gp_Pnt_SquareDistance(gp_Pnt * a, gp_Pnt * b ){
    return a->SquareDistance(*b);
}

void hs_gp_Pnt_BaryCenter(gp_Pnt * a, double alpha, gp_Pnt * b, double beta){
    a->BaryCenter(alpha, *b, beta);
}

bool hs_gp_Pnt_IsEqual(gp_Pnt * a, gp_Pnt * b, double tolerance){
    return a->IsEqual(*b, tolerance);
}

void hs_gp_Pnt_Mirror(gp_Pnt * thePnt, gp_Pnt * theAxis){
    thePnt->Mirror(*theAxis);
}

gp_Pnt * hs_gp_Pnt_Mirrored(gp_Pnt * thePnt, gp_Pnt * theAxis){
    return new gp_Pnt(thePnt->Mirrored(*theAxis));
}

void hs_gp_Pnt_Mirror1(gp_Pnt * thePnt, gp_Ax1 * theAxis){
    thePnt->Mirror(*theAxis);
}

gp_Pnt * hs_gp_Pnt_Mirrored1(gp_Pnt * thePnt, gp_Ax1 * theAxis){
    return new gp_Pnt(thePnt->Mirrored(*theAxis));
}

void hs_gp_Pnt_Mirror2(gp_Pnt * thePnt, gp_Ax1 * theAxis){
    thePnt->Mirror(*theAxis);
}

gp_Pnt * hs_gp_Pnt_Mirrored2(gp_Pnt * thePnt, gp_Ax1 * theAxis){
    return new gp_Pnt(thePnt->Mirrored(*theAxis));
}

void hs_gp_Pnt_Rotate(gp_Pnt * thePnt, gp_Ax1 * theAxis, double amount){
    thePnt->Rotate(*theAxis, amount);
}

gp_Pnt * hs_gp_Pnt_Rotated(gp_Pnt * thePnt, gp_Ax1 * theAxis, double amount){
    return new gp_Pnt(thePnt->Rotated(*theAxis, amount));
}

void hs_gp_Pnt_Scale(gp_Pnt * thePnt, gp_Pnt * origin, double amount){
    thePnt->Scale(*origin, amount);
}

gp_Pnt * hs_gp_Pnt_Scaled(gp_Pnt * thePnt, gp_Pnt * origin, double amount){
    return new gp_Pnt(thePnt->Scaled(*origin, amount));
}

void hs_gp_Pnt_Transform(gp_Pnt * thePnt, gp_Trsf * trsf){
    thePnt->Transform(*trsf);
}

gp_Pnt * hs_gp_Pnt_Transformed(gp_Pnt * thePnt, gp_Trsf * trsf){
    return new gp_Pnt(thePnt->Transformed(*trsf));
}

void hs_gp_Pnt_Translate(gp_Pnt * thePnt, gp_Vec * vec){
    thePnt->Translate(*vec);
}

gp_Pnt * hs_gp_Pnt_Translated(gp_Pnt * thePnt, gp_Vec * vec){
    return new gp_Pnt(thePnt->Translated(*vec));
}

void hs_gp_Pnt_TranslateRelative(gp_Pnt * thePnt, gp_Pnt * from, gp_Pnt * to){
    thePnt->Translate(*from, *to);
}

gp_Pnt * hs_gp_Pnt_TranslatedRelative(gp_Pnt * thePnt, gp_Pnt * from, gp_Pnt * to){
    return new gp_Pnt(thePnt->Translated(*from, *to));
}



