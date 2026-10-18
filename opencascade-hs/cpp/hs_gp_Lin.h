#ifndef HS_GP_LIN_H
#define HS_GP_LIN_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

gp_Lin * hs_new_gp_Lin(gp_Pnt * origin, gp_Dir * direction);

void hs_delete_gp_Lin(gp_Lin* lin);

gp_Pnt * hs_gp_Lin_Location(gp_Lin* lin);

gp_Dir * hs_gp_Lin_Direction(gp_Lin* lin);

#ifdef __cplusplus
}
#endif

#endif // HS_GP_LIN_H
