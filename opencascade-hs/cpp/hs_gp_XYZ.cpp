#include <gp_XYZ.hxx>
#include "hs_gp_XYZ.h"

gp_XYZ * hs_new_gp_XYZ(){
    return new gp_XYZ();
}

gp_XYZ * hs_new_gp_XYZ_fromDoubles(double x, double y, double z){
    return new gp_XYZ(x, y, z);
}

void hs_delete_gp_XYZ(gp_XYZ * xyz){
    delete xyz;
}

void hs_gp_XYZ_setX(gp_XYZ * xyz, double x){
    xyz->SetX(x);
}

void hs_gp_XYZ_setY(gp_XYZ * xyz, double y){
    xyz->SetY(y);
}

void hs_gp_XYZ_setZ(gp_XYZ * xyz, double z){
    xyz->SetZ(z);
}

double hs_gp_XYZ_x(gp_XYZ * xyz){
    return xyz->X();
}

double hs_gp_XYZ_y(gp_XYZ * xyz){
    return xyz->Y();
}

double hs_gp_XYZ_z(gp_XYZ * xyz){
    return xyz->Z();
}

