#include <gp_Dir.hxx>
#include "hs_gp_Dir.h"

gp_Dir * hs_new_gp_Dir(double x, double y, double z) {
    return new gp_Dir(x, y, z);
}

void hs_delete_gp_Dir(gp_Dir* dir){
    delete dir;
}

double hs_gp_Dir_X(gp_Dir * pnt){
    return pnt->X();
}

double hs_gp_Dir_Y(gp_Dir * pnt){
    return pnt->Y();
}

double hs_gp_Dir_Z(gp_Dir * pnt){
    return pnt->Z();
}

void hs_gp_Dir_SetX(gp_Dir * pnt, double x){
    pnt->SetX(x);
}

void hs_gp_Dir_SetY(gp_Dir * pnt, double y){
    pnt->SetY(y);
}

void hs_gp_Dir_SetZ(gp_Dir * pnt, double z){
    pnt->SetZ(z);
}

bool hs_gp_Dir_IsEqual(gp_Dir * a, gp_Dir * b, double angularTolerance){
    return a->IsEqual(*b, angularTolerance);
}

bool hs_gp_Dir_IsNormal(gp_Dir * a, gp_Dir * b, double angularTolerance){
    return a->IsNormal(*b, angularTolerance);
}

bool hs_gp_Dir_IsOpposite(gp_Dir * a, gp_Dir * b, double angularTolerance){
    return a->IsOpposite(*b, angularTolerance);
}

bool hs_gp_Dir_IsParallel(gp_Dir * a, gp_Dir * b, double angularTolerance){
    return a->IsParallel(*b, angularTolerance);
}

double hs_gp_Dir_Angle(gp_Dir * a, gp_Dir * b){
    return a->Angle(*b);
}

double hs_gp_Dir_AngleWithRef(gp_Dir * a, gp_Dir * b, gp_Dir* theVRef){
    return a->AngleWithRef(*b, *theVRef);
}

void hs_gp_Dir_Cross(gp_Dir * a, gp_Dir * b){
    a->Cross(*b);
}

gp_Dir * hs_gp_Dir_Crossed(gp_Dir * a, gp_Dir * b){
    return new gp_Dir(a->Crossed(*b));
}

void hs_gp_Dir_CrossCross(gp_Dir * a, gp_Dir * b, gp_Dir * c){
    return a->CrossCross(*b, *c);
}

gp_Dir * hs_gp_Dir_CrossCrossed(gp_Dir * a, gp_Dir * b, gp_Dir * c){
    return new gp_Dir(a->CrossCrossed(*b, *c));
}

double hs_gp_Dir_Dot(gp_Dir * a, gp_Dir * b){
    return a->Dot(*b);
}

double hs_gp_Dir_DotCross(gp_Dir * a, gp_Dir * b, gp_Dir * c){
    return a->DotCross(*b, *c);
}


void hs_gp_Dir_Reverse(gp_Dir* ax1){
    ax1->Reverse();
}

gp_Dir * hs_gp_Dir_Reversed(gp_Dir* ax1){
    return new gp_Dir(ax1->Reversed());
}

void hs_gp_Dir_Mirror(gp_Dir * theDir, gp_Dir * theAxis){
    theDir->Mirror(*theAxis);
}

gp_Dir * hs_gp_Dir_Mirrored(gp_Dir * theDir, gp_Dir * theAxis){
    return new gp_Dir(theDir->Mirrored(*theAxis));
}

void hs_gp_Dir_MirrorAboutAx1(gp_Dir * theDir, gp_Ax1 * theAxis){
    theDir->Mirror(*theAxis);
}

gp_Dir * hs_gp_Dir_MirroredAboutAx1(gp_Dir * theDir, gp_Ax1 * theAxis){
    return new gp_Dir(theDir->Mirrored(*theAxis));
}

void hs_gp_Dir_MirrorAboutAx2(gp_Dir * theDir, gp_Ax2 * theAxis){
    theDir->Mirror(*theAxis);
}

gp_Dir * hs_gp_Dir_MirroredAboutAx2(gp_Dir * theDir, gp_Ax2 * theAxis){
    return new gp_Dir(theDir->Mirrored(*theAxis));
}

void hs_gp_Dir_Rotate(gp_Dir * theDir, gp_Ax1 * theAxis, double amount){
    theDir->Rotate(*theAxis, amount);
}

gp_Dir * hs_gp_Dir_Rotated(gp_Dir * theDir, gp_Ax1 * theAxis, double amount){
    return new gp_Dir(theDir->Rotated(*theAxis, amount));
}

void hs_gp_Dir_Transform(gp_Dir * theDir, gp_Trsf * trsf){
    theDir->Transform(*trsf);
}

gp_Dir * hs_gp_Dir_Transformed(gp_Dir * theDir, gp_Trsf * trsf){
    return new gp_Dir(theDir->Transformed(*trsf));
}
