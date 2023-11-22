#include <gp_Trsf2d.hxx>
#include "hs_gp_Trsf2d.h"

gp_Trsf2d * hs_new_gp_Trsf2d(){
    return new gp_Trsf2d();
}

void hs_delete_gp_Trsf2d(gp_Trsf2d * t){
    delete t;
}

gp_Trsf2d * hs_new_gp_Trsf2d_fromTrsf(gp_Trsf * t){
    return new gp_Trsf2d(*t);
}

void hs_gp_Trsf2d_SetMirrorAboutPnt2d(gp_Trsf2d * trsf, gp_Pnt2d * pnt){
    trsf->SetMirror(*pnt);
}

void hs_gp_Trsf2d_SetMirrorAboutAx2d(gp_Trsf2d * trsf, gp_Ax2d * ax){
    trsf->SetMirror(*ax);
}

void hs_gp_Trsf2d_SetRotation(gp_Trsf2d * trsf, gp_Pnt2d * ax, double angle){
    trsf->SetRotation(*ax, angle);
}

void hs_gp_Trsf2d_SetScale(gp_Trsf2d * trsf, gp_Pnt2d * origin, double factor){
    trsf->SetScale(*origin, factor);
}

void hs_gp_Trsf2d_SetTransformation(gp_Trsf2d * trsf, gp_Ax2d * to){
    trsf->SetTransformation(*to);
}

void hs_gp_Trsf2d_SetTransformationRelative(gp_Trsf2d * trsf, gp_Ax2d * from, gp_Ax2d * to){
    trsf->SetTransformation(*from, *to);
}

void hs_gp_Trsf2d_SetTranslation(gp_Trsf2d * trsf, gp_Vec2d * trans){
    trsf->SetTranslation(*trans);
}

void hs_gp_Trsf2d_SetTranslationRelative(gp_Trsf2d * trsf, gp_Pnt2d * from, gp_Pnt2d * to){
    trsf->SetTranslation(*from, *to);
}

void hs_gp_Trsf2d_SetTranslationPart(gp_Trsf2d * trsf, gp_Vec2d * trans){
    trsf->SetTranslationPart(*trans);
}

void hs_gp_Trsf2d_SetScaleFactor(gp_Trsf2d * trsf, double s){
    trsf->SetScaleFactor(s);
}

void hs_gp_Trsf2d_SetValues(gp_Trsf2d * trsf, 
        double a11, double a12, double a13, 
        double a21, double a22, double a23){
    trsf->SetValues(
         a11, a12, a13,
         a21, a22, a23
    );
}

bool hs_gp_Trsf2d_IsNegative(gp_Trsf2d * trsf){
    return trsf->IsNegative();
}

double hs_gp_Trsf2d_ScaleFactor(gp_Trsf2d * trsf){
    return trsf->ScaleFactor();
}

double hs_gp_Trsf2d_Value(gp_Trsf2d* trsf, int row, int col){
    return trsf->Value(row, col);
}

void hs_gp_Trsf2d_Invert(gp_Trsf2d* trsf){
    trsf->Invert();
}

gp_Trsf2d * hs_gp_Trsf2d_Inverted(gp_Trsf2d* trsf){
    return new gp_Trsf2d(trsf->Inverted());
}

void hs_gp_Trsf2d_Multiply(gp_Trsf2d * trsf, gp_Trsf2d* b){
    trsf->Multiply(*b);
}

gp_Trsf2d * hs_gp_Trsf2d_Multiplied(gp_Trsf2d* a, gp_Trsf2d* b){
    return new gp_Trsf2d(a->Multiplied(*b));
}

void hs_gp_Trsf2d_PreMultiply(gp_Trsf2d * trsf, gp_Trsf2d* b){
    trsf->PreMultiply(*b);
}

void hs_gp_Trsf2d_Power(gp_Trsf2d * trsf, int b){
    trsf->Power(b);
}

gp_Trsf2d * hs_gp_Trsf2d_Powered(gp_Trsf2d* a, int b){
    return new gp_Trsf2d(a->Powered(b));
}

