#include <gp_Pln.hxx>
#include "hs_Exception.h"
#include "hs_gp_Pln.h"

gp_Pln * hs_new_gp_Pln(){
    return new gp_Pln();
}

void hs_delete_gp_Pln(gp_Pln * pln) {
    delete pln;
}

gp_Pln * hs_new_gp_Pln_fromAx3(gp_Ax3 * ax){
    return new gp_Pln(*ax);
}

gp_Pln * hs_new_gp_Pln_fromPntAndDir(gp_Pnt * pnt, gp_Dir * dir){
    return new gp_Pln(*pnt, *dir);
}

gp_Pln * hs_new_gp_Pln_fromCartesianEquation(double theA, double theB, double theC, double theD, HSExceptionType* exType, void** exPtr) {
    return hs_handleEx(exType, exPtr, [theA, theB, theC, theD]{
        return new gp_Pln(theA, theB, theC, theD);
    });
}

void hs_gp_Pln_SetAxis(gp_Pln * pln, gp_Ax1 *ax, HSExceptionType* exType, void** exPtr) {
    hs_handleExVoid(exType, exPtr, [pln, ax]{
        pln->SetAxis(*ax);
    });
}

void hs_gp_Pln_SetLocation(gp_Pln * pln, gp_Pnt * loc){
    pln->SetLocation(*loc);
}

void hs_gp_Pln_SetPosition(gp_Pln * pln, gp_Ax3 * pos){
    pln->SetPosition(*pos);
}

void hs_gp_Pln_VReverse(gp_Pln *pln){
    pln->VReverse();
}

void hs_gp_Pln_UReverse(gp_Pln *pln){
    pln->UReverse();
}

bool hs_gp_Pln_Direct(gp_Pln *pln){
    return pln->Direct();
}

gp_Ax1 * hs_gp_Pln_Axis (gp_Pln *pln){
    return new gp_Ax1(pln->Axis());
}

gp_Pnt * hs_gp_Pln_Location(gp_Pln * pln){
    return new gp_Pnt(pln->Location());
}

gp_Ax3 * hs_gp_Pln_Position(gp_Pln * pln){
    return new gp_Ax3(pln->Position());
}

double hs_gp_Pln_DistancePnt(gp_Pln * pln, gp_Pnt* pnt) {
    return pln->Distance(*pnt);
}

double hs_gp_Pln_DistanceLin(gp_Pln * pln, gp_Lin* lin) {
    return pln->Distance(*lin);
}

double hs_gp_Pln_DistancePln(gp_Pln * pln, gp_Pln* other) {
    return pln->Distance(*other);
}


double hs_gp_Pln_SquareDistancePnt(gp_Pln * pln, gp_Pnt* pnt) {
    return pln->SquareDistance(*pnt);
}

double hs_gp_Pln_SquareDistanceLin(gp_Pln * pln, gp_Lin* lin) {
    return pln->SquareDistance(*lin);
}

double hs_gp_Pln_SquareDistancePln(gp_Pln * pln, gp_Pln* other) {
    return pln->SquareDistance(*other);
}

gp_Ax1 * hs_gp_Pln_XAxis(gp_Pln * pln) {
    return new gp_Ax1(pln->XAxis());
}

gp_Ax1 * hs_gp_Pln_YAxis(gp_Pln * pln) {
    return new gp_Ax1(pln->YAxis());
}

bool hs_gp_Pln_ContainsPnt(gp_Pln * pln, gp_Pnt* pnt, double tolerance){
    return pln->Contains(*pnt, tolerance);
}

bool hs_gp_Pln_ContainsLin(gp_Pln * pln, gp_Lin* lin, double linearTolerance, double angularTolerance){
    return pln->Contains(*lin, linearTolerance, angularTolerance);
}

void hs_gp_Pln_Coefficients(gp_Pln * pln, double * a, double * b, double * c, double * d){
    pln->Coefficients(*a, *b, *c, *d);
}

void hs_gp_Pln_Mirror(gp_Pln * pln, gp_Pnt * pnt){
    pln->Mirror(*pnt);
}

gp_Pln * hs_gp_Pln_Mirrored(gp_Pln * pln, gp_Pnt * pnt){
    return new gp_Pln(pln->Mirrored(*pnt));
}

void hs_gp_Pln_MirrorAboutAx1(gp_Pln * pln, gp_Ax1 * axis){
    pln->Mirror(*axis);
}

gp_Pln * hs_gp_Pln_MirroredAboutAx1(gp_Pln * pln, gp_Ax1 * axis){
    return new gp_Pln(pln->Mirrored(*axis));
}

void hs_gp_Pln_MirrorAboutAx2(gp_Pln * pln, gp_Ax2 * axis){
    pln->Mirror(*axis);
}

gp_Pln * hs_gp_Pln_MirroredAboutAx2(gp_Pln * pln, gp_Ax2 * axis){
    return new gp_Pln(pln->Mirrored(*axis));
}

void hs_gp_Pln_Rotate(gp_Pln * pln, gp_Ax1 * axis, double amount){
    pln->Rotate(*axis, amount);
}

gp_Pln * hs_gp_Pln_Rotated(gp_Pln * pln, gp_Ax1 * axis, double amount){
    return new gp_Pln(pln->Rotated(*axis, amount));
}

void hs_gp_Pln_Scale(gp_Pln * pln, gp_Pnt * origin, double amount){
    pln->Scale(*origin, amount);
}

gp_Pln * hs_gp_Pln_Scaled(gp_Pln * pln, gp_Pnt * origin, double amount){
    return new gp_Pln(pln->Scaled(*origin, amount));
}

void hs_gp_Pln_Transform(gp_Pln * pln, gp_Trsf * trsf){
    pln->Transform(*trsf);
}

gp_Pln * hs_gp_Pln_Transformed(gp_Pln * pln, gp_Trsf * trsf){
    return new gp_Pln(pln->Transformed(*trsf));
}

void hs_gp_Pln_Translate(gp_Pln * pln, gp_Vec * vec){
    pln->Translate(*vec);
}

gp_Pln * hs_gp_Pln_Translated(gp_Pln * pln, gp_Vec * vec){
    return new gp_Pln(pln->Translated(*vec));
}

void hs_gp_Pln_TranslateRelative(gp_Pln * pln, gp_Pnt * from, gp_Pnt * to){
    pln->Translate(*from, *to);
}

gp_Pln * hs_gp_Pln_TranslatedRelative(gp_Pln * pln, gp_Pnt * from, gp_Pnt * to){
    return new gp_Pln(pln->Translated(*from, *to));
}
