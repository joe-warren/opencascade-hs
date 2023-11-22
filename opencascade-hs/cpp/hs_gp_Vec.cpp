#include <gp_Vec.hxx>
#include "hs_gp_Vec.h"

gp_Vec * hs_new_gp_Vec(double x, double y, double z) {
    return new gp_Vec(x, y, z);
}

void hs_delete_gp_Vec(gp_Vec* vec){
    delete vec;
}


double hs_gp_Vec_X(gp_Vec * vec){
    return vec->X();
}

double hs_gp_Vec_Y(gp_Vec * vec){
    return vec->Y();
}

double hs_gp_Vec_Z(gp_Vec * vec){
    return vec->Z();
}

void hs_gp_Vec_SetX(gp_Vec * vec, double x){
    vec->SetX(x);
}

void hs_gp_Vec_SetY(gp_Vec * vec, double y){
    vec->SetY(y);
}

void hs_gp_Vec_SetZ(gp_Vec * vec, double z){
    vec->SetZ(z);
}

bool hs_gp_Vec_IsEqual(gp_Vec * a, gp_Vec * b, double linearTolerance, double angularTolerance){
    return a->IsEqual(*b, linearTolerance, angularTolerance);
}

bool hs_gp_Vec_IsNormal(gp_Vec * a, gp_Vec * b, double angularTolerance){
    return a->IsNormal(*b, angularTolerance);
}

bool hs_gp_Vec_IsOpposite(gp_Vec * a, gp_Vec * b, double angularTolerance){
    return a->IsOpposite(*b, angularTolerance);
}

bool hs_gp_Vec_IsParallel(gp_Vec * a, gp_Vec * b, double angularTolerance){
    return a->IsParallel(*b, angularTolerance);
}

double hs_gp_Vec_Angle(gp_Vec * a, gp_Vec * b){
    return a->Angle(*b);
}

double hs_gp_Vec_AngleWithRef(gp_Vec * a, gp_Vec * b, gp_Vec* theVRef){
    return a->AngleWithRef(*b, *theVRef);
}

double hs_gp_Vec_Magnitude(gp_Vec * a){
    return a->Magnitude();
}

double hs_gp_Vec_SquareMagnitude(gp_Vec * a){
    return a->SquareMagnitude();
}

void hs_gp_Vec_Add(gp_Vec * a, gp_Vec * b){
    a->Add(*b);
}

gp_Vec * hs_gp_Vec_Added(gp_Vec * a, gp_Vec * b){
    return new gp_Vec(a->Added(*b));
}


void hs_gp_Vec_Subtract(gp_Vec * a, gp_Vec * b){
    a->Subtract(*b);
}

gp_Vec * hs_gp_Vec_Subtracted(gp_Vec * a, gp_Vec * b){
    return new gp_Vec(a->Subtracted(*b));
}

void hs_gp_Vec_Multiply(gp_Vec * a, double b){
    a->Multiply(b);
}

gp_Vec * hs_gp_Vec_Multiplied(gp_Vec * a, double b){
    return new gp_Vec(a->Multiplied(b));
}


void hs_gp_Vec_Divide(gp_Vec * a, double b){
    a->Divide(b);
}

gp_Vec * hs_gp_Vec_Divided(gp_Vec * a, double b){
    return new gp_Vec(a->Divided(b));
}

void hs_gp_Vec_Cross(gp_Vec * a, gp_Vec * b){
    a->Cross(*b);
}

gp_Vec * hs_gp_Vec_Crossed(gp_Vec * a, gp_Vec * b){
    return new gp_Vec(a->Crossed(*b));
}

void hs_gp_Vec_CrossCross(gp_Vec * a, gp_Vec * b, gp_Vec * c){
    return a->CrossCross(*b, *c);
}

double hs_gp_Vec_CrossMagnitude(gp_Vec * a, gp_Vec * b){
    return a->CrossMagnitude(*b);
}

double hs_gp_Vec_CrossSquareMagnitude(gp_Vec * a, gp_Vec * b){
    return a->CrossSquareMagnitude(*b);
}

gp_Vec * hs_gp_Vec_CrossCrossed(gp_Vec * a, gp_Vec * b, gp_Vec * c){
    return new gp_Vec(a->CrossCrossed(*b, *c));
}

double hs_gp_Vec_Dot(gp_Vec * a, gp_Vec * b){
    return a->Dot(*b);
}

double hs_gp_Vec_DotCross(gp_Vec * a, gp_Vec * b, gp_Vec * c){
    return a->DotCross(*b, *c);
}

void hs_gp_Vec_Normalize(gp_Vec * a){
    a->Normalize();
}

gp_Vec * hs_gp_Vec_Normalized(gp_Vec * a){
    return new gp_Vec(a->Normalized());
}

void hs_gp_Vec_Reverse(gp_Vec* a){
    a->Reverse();
}

gp_Vec * hs_gp_Vec_Reversed(gp_Vec* a){
    return new gp_Vec(a->Reversed());
}

void hs_gp_Vec_Mirror(gp_Vec * theVec, gp_Vec * theAxis){
    theVec->Mirror(*theAxis);
}

gp_Vec * hs_gp_Vec_Mirrored(gp_Vec * theVec, gp_Vec * theAxis){
    return new gp_Vec(theVec->Mirrored(*theAxis));
}

void hs_gp_Vec_MirrorAboutAx1(gp_Vec * theVec, gp_Ax1 * theAxis){
    theVec->Mirror(*theAxis);
}

gp_Vec * hs_gp_Vec_MirroredAboutAx1(gp_Vec * theVec, gp_Ax1 * theAxis){
    return new gp_Vec(theVec->Mirrored(*theAxis));
}

void hs_gp_Vec_MirrorAboutAx2(gp_Vec * theVec, gp_Ax2 * theAxis){
    theVec->Mirror(*theAxis);
}

gp_Vec * hs_gp_Vec_MirroredAboutAx2(gp_Vec * theVec, gp_Ax2 * theAxis){
    return new gp_Vec(theVec->Mirrored(*theAxis));
}

void hs_gp_Vec_Rotate(gp_Vec * theVec, gp_Ax1 * theAxis, double amount){
    theVec->Rotate(*theAxis, amount);
}

gp_Vec * hs_gp_Vec_Rotated(gp_Vec * theVec, gp_Ax1 * theAxis, double amount){
    return new gp_Vec(theVec->Rotated(*theAxis, amount));
}

void hs_gp_Vec_Scale(gp_Vec * theVec, double s){
    theVec->Scale(s);
}

gp_Vec * hs_gp_Vec_Scaled(gp_Vec * theVec, double s){
    return new gp_Vec(theVec->Scaled(s));
}

void hs_gp_Vec_Transform(gp_Vec * theVec, gp_Trsf * trsf){
    theVec->Transform(*trsf);
}

gp_Vec * hs_gp_Vec_Transformed(gp_Vec * theVec, gp_Trsf * trsf){
    return new gp_Vec(theVec->Transformed(*trsf));
}
