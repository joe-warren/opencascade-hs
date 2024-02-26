#ifndef HS_BND_OBB_H
#define HS_BND_OBB_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

Bnd_OBB * hs_new_Bnd_OBB();

void hs_delete_Bnd_OBB(Bnd_OBB * obb);

gp_XYZ * hs_Bnd_OBB_center(Bnd_OBB * obb);

gp_XYZ * hs_Bnd_OBB_xDirection(Bnd_OBB * obb);

gp_XYZ * hs_Bnd_OBB_yDirection(Bnd_OBB * obb);

gp_XYZ * hs_Bnd_OBB_zDirection(Bnd_OBB * obb);

double hs_Bnd_OBB_xHSize(Bnd_OBB *obb);

double hs_Bnd_OBB_yHSize(Bnd_OBB *obb);

double hs_Bnd_OBB_zHSize(Bnd_OBB *obb);

gp_Ax3 * hs_Bnd_OBB_position(Bnd_OBB *obb);

#ifdef __cplusplus
}
#endif

#endif // HS_BND_OBB_H
