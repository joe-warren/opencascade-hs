#include <Bnd_OBB.hxx>
#include "hs_Bnd_OBB.h"

Bnd_OBB * hs_new_Bnd_OBB() {
    return new Bnd_OBB();
}

void hs_delete_Bnd_OBB(Bnd_OBB * obb){
    delete obb;
}

gp_XYZ * hs_Bnd_OBB_center(Bnd_OBB * obb){
    return new gp_XYZ(obb->Center());
}

gp_XYZ * hs_Bnd_OBB_xDirection(Bnd_OBB * obb){
    return new gp_XYZ(obb->XDirection());
}

gp_XYZ * hs_Bnd_OBB_yDirection(Bnd_OBB * obb){
    return new gp_XYZ(obb->YDirection());
}

gp_XYZ * hs_Bnd_OBB_zDirection(Bnd_OBB * obb){
    return new gp_XYZ(obb->ZDirection());
}

double hs_Bnd_OBB_xHSize(Bnd_OBB *obb){
    return obb->XHSize();
}

double hs_Bnd_OBB_yHSize(Bnd_OBB *obb){
    return obb->YHSize();
}

double hs_Bnd_OBB_zHSize(Bnd_OBB *obb){
    return obb->ZHSize();
}

gp_Ax3 * hs_Bnd_OBB_position(Bnd_OBB *obb){
    return new gp_Ax3(obb->Position());
}