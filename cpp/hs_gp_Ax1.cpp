#include <gp_Ax1.hxx>
#include "hs_gp_Ax1.h"

gp_Ax1 * hs_new_gp_Ax1(gp_Pnt * origin, gp_Dir * direction){
    return new gp_Ax1(*origin, *direction);
}

void hs_delete_gp_Ax1(gp_Ax1* ax1){
    delete ax1;
}

gp_Dir * hs_gp_Ax1_Direction(gp_Ax1* ax1){
    return new gp_Dir(ax1->Direction());
}

gp_Pnt * hs_gp_Ax1_Location(gp_Ax1* ax1){
    return new gp_Pnt(ax1->Location());
}

void hs_gp_Ax1_SetDirection(gp_Ax1* ax1, gp_Dir* direction){
    ax1->SetDirection(*direction);
}

void hs_gp_Ax1_SetLocation(gp_Ax1* ax1, gp_Pnt* origin){
    ax1->SetLocation(*origin);
}
