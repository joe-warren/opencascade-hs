#include <gp_Trsf.hxx>
#include "hs_gp_Trsf.h"

gp_Trsf * hs_new_gp_Trsf(){
    return new gp_Trsf();
}

void hs_delete_gp_Trsf(gp_Trsf * t){
    delete t;
}

gp_Trsf * hs_new_gp_Trsf_fromTrsf2d(gp_Trsf2d * t){
    return new gp_Trsf(*t);
}

void hs_gp_Trsf_SetMirrorAboutPnt(gp_Trsf * trsf, gp_Pnt * pnt){
    trsf->SetMirror(*pnt);
}

void hs_gp_Trsf_SetMirrorAboutAx1(gp_Trsf * trsf, gp_Ax1 * ax){
    trsf->SetMirror(*ax);
}

void hs_gp_Trsf_SetMirrorAboutAx2(gp_Trsf * trsf, gp_Ax2 * ax){
    trsf->SetMirror(*ax);
}

void hs_gp_Trsf_SetRotationAboutAxisAngle(gp_Trsf * trsf, gp_Ax1 * ax, double angle){
    trsf->SetRotation(*ax, angle);
}

void hs_gp_Trsf_SetScale(gp_Trsf * trsf, gp_Pnt * origin, double factor){
    trsf->SetScale(*origin, factor);
}

void hs_gp_Trsf_SetTranslation(gp_Trsf * trsf, gp_Vec * trans){
    trsf->SetTranslation(*trans);
}

void hs_gp_Trsf_SetTranslationPart(gp_Trsf * trsf, gp_Vec * trans){
    trsf->SetTranslationPart(*trans);
}

void hs_gp_Trsf_SetScaleFactor(gp_Trsf * trsf, double s){
    trsf->SetScaleFactor(s);
}

void hs_gp_Trsf_SetValues(gp_Trsf * trsf, 
        double a11, double a12, double a13, double a14,
        double a21, double a22, double a23, double a24,
        double a31, double a32, double a33, double a34){
    trsf->SetValues(
         a11, a12, a13, a14,
         a21, a22, a23, a24,
         a31, a32, a33, a34
    );
}

bool hs_gp_Trsf_IsNegative(gp_Trsf * trsf){
    return trsf->IsNegative();
}

double hs_gp_Trsf_ScaleFactor(gp_Trsf * trsf){
    return trsf->ScaleFactor();
}

double hs_gp_Trsf_Value(gp_Trsf* trsf, int row, int col){
    return trsf->Value(row, col);
}

void hs_gp_Trsf_Invert(gp_Trsf* trsf){
    trsf->Invert();
}

gp_Trsf * hs_gp_Trsf_Inverted(gp_Trsf* trsf){
    return new gp_Trsf(trsf->Inverted());
}

void hs_gp_Trsf_Multiply(gp_Trsf * trsf, gp_Trsf* b){
    trsf->Multiply(*b);
}

gp_Trsf * hs_gp_Trsf_Multiplied(gp_Trsf* a, gp_Trsf* b){
    return new gp_Trsf(a->Multiplied(*b));
}

void hs_gp_Trsf_PreMultiply(gp_Trsf * trsf, gp_Trsf* b){
    trsf->PreMultiply(*b);
}

void hs_gp_Trsf_Power(gp_Trsf * trsf, int b){
    trsf->Power(b);
}

gp_Trsf * hs_gp_Trsf_Powered(gp_Trsf* a, int b){
    return new gp_Trsf(a->Powered(b));
}

