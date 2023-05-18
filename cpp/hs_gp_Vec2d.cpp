#include <gp_Vec2d.hxx>
#include "hs_gp_Vec2d.h"

gp_Vec2d * hs_new_gp_Vec2d(double x, double y) {
    return new gp_Vec2d(x, y);
}

void hs_delete_gp_Vec2d(gp_Vec2d* vec){
    delete vec;
}

double hs_gp_Vec2d_X(gp_Vec2d * vec){
    return vec->X();
}

double hs_gp_Vec2d_Y(gp_Vec2d * vec){
    return vec->Y();
}

void hs_gp_Vec2d_SetX(gp_Vec2d * vec, double x){
    vec->SetX(x);
}

void hs_gp_Vec2d_SetY(gp_Vec2d * vec, double y){
    vec->SetY(y);
}

bool hs_gp_Vec2d_IsEqual(gp_Vec2d * a, gp_Vec2d * b, double linearTolerance, double angularTolerance){
    return a->IsEqual(*b, linearTolerance, angularTolerance);
}

bool hs_gp_Vec2d_IsNormal(gp_Vec2d * a, gp_Vec2d * b, double angularTolerance){
    return a->IsNormal(*b, angularTolerance);
}

bool hs_gp_Vec2d_IsOpposite(gp_Vec2d * a, gp_Vec2d * b, double angularTolerance){
    return a->IsOpposite(*b, angularTolerance);
}

bool hs_gp_Vec2d_IsParallel(gp_Vec2d * a, gp_Vec2d * b, double angularTolerance){
    return a->IsParallel(*b, angularTolerance);
}

double hs_gp_Vec2d_Angle(gp_Vec2d * a, gp_Vec2d * b){
    return a->Angle(*b);
}

double hs_gp_Vec2d_Magnitude(gp_Vec2d * a){
    return a->Magnitude();
}

double hs_gp_Vec2d_SquareMagnitude(gp_Vec2d * a){
    return a->SquareMagnitude();
}

void hs_gp_Vec2d_Add(gp_Vec2d * a, gp_Vec2d * b){
    a->Add(*b);
}

gp_Vec2d * hs_gp_Vec2d_Added(gp_Vec2d * a, gp_Vec2d * b){
    return new gp_Vec2d(a->Added(*b));
}

void hs_gp_Vec2d_Subtract(gp_Vec2d * a, gp_Vec2d * b){
    a->Subtract(*b);
}

gp_Vec2d * hs_gp_Vec2d_Subtracted(gp_Vec2d * a, gp_Vec2d * b){
    return new gp_Vec2d(a->Subtracted(*b));
}

void hs_gp_Vec2d_Multiply(gp_Vec2d * a, double b){
    a->Multiply(b);
}

gp_Vec2d * hs_gp_Vec2d_Multiplied(gp_Vec2d * a, double b){
    return new gp_Vec2d(a->Multiplied(b));
}


void hs_gp_Vec2d_Divide(gp_Vec2d * a, double b){
    a->Divide(b);
}

gp_Vec2d * hs_gp_Vec2d_Divided(gp_Vec2d * a, double b){
    return new gp_Vec2d(a->Divided(b));
}

double hs_gp_Vec2d_Crossed(gp_Vec2d * a, gp_Vec2d * b){
    return a->Crossed(*b);
}

double hs_gp_Vec2d_CrossMagnitude(gp_Vec2d * a, gp_Vec2d * b){
    return a->CrossMagnitude(*b);
}

double hs_gp_Vec2d_CrossSquareMagnitude(gp_Vec2d * a, gp_Vec2d * b){
    return a->CrossSquareMagnitude(*b);
}

double hs_gp_Vec2d_Dot(gp_Vec2d * a, gp_Vec2d * b){
    return a->Dot(*b);
}

void hs_gp_Vec2d_Normalize(gp_Vec2d * a){
    a->Normalize();
}

gp_Vec2d * hs_gp_Vec2d_Normalized(gp_Vec2d * a){
    return new gp_Vec2d(a->Normalized());
}

void hs_gp_Vec2d_Reverse(gp_Vec2d* a){
    a->Reverse();
}

gp_Vec2d * hs_gp_Vec2d_Reversed(gp_Vec2d* a){
    return new gp_Vec2d(a->Reversed());
}

void hs_gp_Vec2d_Mirror(gp_Vec2d * theVec2d, gp_Vec2d * theAxis){
    theVec2d->Mirror(*theAxis);
}

gp_Vec2d * hs_gp_Vec2d_Mirrored(gp_Vec2d * theVec2d, gp_Vec2d * theAxis){
    return new gp_Vec2d(theVec2d->Mirrored(*theAxis));
}

void hs_gp_Vec2d_MirrorAboutAx2d(gp_Vec2d * theVec2d, gp_Ax2d * theAxis){
    theVec2d->Mirror(*theAxis);
}

gp_Vec2d * hs_gp_Vec2d_MirroredAboutAx2d(gp_Vec2d * theVec2d, gp_Ax2d * theAxis){
    return new gp_Vec2d(theVec2d->Mirrored(*theAxis));
}

void hs_gp_Vec2d_Rotate(gp_Vec2d * theVec2d, double amount){
    theVec2d->Rotate(amount);
}

gp_Vec2d * hs_gp_Vec2d_Rotated(gp_Vec2d * theVec2d, double amount){
    return new gp_Vec2d(theVec2d->Rotated(amount));
}

void hs_gp_Vec2d_Scale(gp_Vec2d * theVec2d, double s){
    theVec2d->Scale(s);
}

gp_Vec2d * hs_gp_Vec2d_Scaled(gp_Vec2d * theVec2d, double s){
    return new gp_Vec2d(theVec2d->Scaled(s));
}

void hs_gp_Vec2d_Transform(gp_Vec2d * theVec2d, gp_Trsf2d * trsf){
    theVec2d->Transform(*trsf);
}

gp_Vec2d * hs_gp_Vec2d_Transformed(gp_Vec2d * theVec2d, gp_Trsf2d * trsf){
    return new gp_Vec2d(theVec2d->Transformed(*trsf));
}
