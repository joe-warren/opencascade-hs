#include <gp_Pnt2d.hxx>
#include "hs_gp_Pnt2d.h"

gp_Pnt2d * hs_new_gp_Pnt2d(double x, double y) {
    return new gp_Pnt2d(x, y);
}

void hs_delete_gp_Pnt2d(gp_Pnt2d* pnt){
    delete pnt;
}

double hs_gp_Pnt2d_X(gp_Pnt2d * pnt){
    return pnt->X();
}

double hs_gp_Pnt2d_Y(gp_Pnt2d * pnt){
    return pnt->Y();
}

void hs_gp_Pnt2d_SetX(gp_Pnt2d * pnt, double x){
    pnt->SetX(x);
}

void hs_gp_Pnt2d_SetY(gp_Pnt2d * pnt, double y){
    pnt->SetY(y);
}

double hs_gp_Pnt2d_Distance(gp_Pnt2d * a, gp_Pnt2d * b ){
    return a->Distance(*b);
}

double hs_gp_Pnt2d_SquareDistance(gp_Pnt2d * a, gp_Pnt2d * b ){
    return a->SquareDistance(*b);
}

bool hs_gp_Pnt2d_IsEqual(gp_Pnt2d * a, gp_Pnt2d * b, double tolerance){
    return a->IsEqual(*b, tolerance);
}

void hs_gp_Pnt2d_Mirror(gp_Pnt2d * thePnt2d, gp_Pnt2d * theAxis){
    thePnt2d->Mirror(*theAxis);
}

gp_Pnt2d * hs_gp_Pnt2d_Mirrored(gp_Pnt2d * thePnt2d, gp_Pnt2d * theAxis){
    return new gp_Pnt2d(thePnt2d->Mirrored(*theAxis));
}

void hs_gp_Pnt2d_MirrorAboutAx2d(gp_Pnt2d * thePnt2d, gp_Ax2d * theAxis){
    thePnt2d->Mirror(*theAxis);
}

gp_Pnt2d * hs_gp_Pnt2d_MirroredAboutAx2d(gp_Pnt2d * thePnt2d, gp_Ax2d * theAxis){
    return new gp_Pnt2d(thePnt2d->Mirrored(*theAxis));
}

void hs_gp_Pnt2d_Rotate(gp_Pnt2d * thePnt2d, gp_Pnt2d * theAxis, double amount){
    thePnt2d->Rotate(*theAxis, amount);
}

gp_Pnt2d * hs_gp_Pnt2d_Rotated(gp_Pnt2d * thePnt2d, gp_Pnt2d * theAxis, double amount){
    return new gp_Pnt2d(thePnt2d->Rotated(*theAxis, amount));
}

void hs_gp_Pnt2d_Scale(gp_Pnt2d * thePnt2d, gp_Pnt2d * origin, double amount){
    thePnt2d->Scale(*origin, amount);
}

gp_Pnt2d * hs_gp_Pnt2d_Scaled(gp_Pnt2d * thePnt2d, gp_Pnt2d * origin, double amount){
    return new gp_Pnt2d(thePnt2d->Scaled(*origin, amount));
}

void hs_gp_Pnt2d_Transform(gp_Pnt2d * thePnt2d, gp_Trsf2d * trsf){
    thePnt2d->Transform(*trsf);
}

gp_Pnt2d * hs_gp_Pnt2d_Transformed(gp_Pnt2d * thePnt2d, gp_Trsf2d * trsf){
    return new gp_Pnt2d(thePnt2d->Transformed(*trsf));
}

void hs_gp_Pnt2d_Translate(gp_Pnt2d * thePnt2d, gp_Vec2d * vec){
    thePnt2d->Translate(*vec);
}

gp_Pnt2d * hs_gp_Pnt2d_Translated(gp_Pnt2d * thePnt2d, gp_Vec2d * vec){
    return new gp_Pnt2d(thePnt2d->Translated(*vec));
}

void hs_gp_Pnt2d_TranslateRelative(gp_Pnt2d * thePnt2d, gp_Pnt2d * from, gp_Pnt2d * to){
    thePnt2d->Translate(*from, *to);
}

gp_Pnt2d * hs_gp_Pnt2d_TranslatedRelative(gp_Pnt2d * thePnt2d, gp_Pnt2d * from, gp_Pnt2d * to){
    return new gp_Pnt2d(thePnt2d->Translated(*from, *to));
}

