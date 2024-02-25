#ifndef HS_BND_BOX_H
#define HS_BND_BOX_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

Bnd_Box * hs_new_Bnd_Box();

void hs_delete_Bnd_Box(Bnd_Box * box);

gp_Pnt * hs_Bnd_Box_cornerMin(Bnd_Box * box);

gp_Pnt * hs_Bnd_Box_cornerMax(Bnd_Box * box);

#ifdef __cplusplus
}
#endif

#endif // HS_BND_BOX_H
