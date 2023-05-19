#ifndef HS_GP_H
#define HS_GP_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

gp_Pnt * hs_gp_Origin(); 

gp_Dir * hs_gp_DX(); 

gp_Dir * hs_gp_DY(); 

gp_Dir * hs_gp_DZ(); 

gp_Ax1 * hs_gp_OX(); 

gp_Ax1 * hs_gp_OY(); 

gp_Ax1 * hs_gp_OZ(); 

gp_Ax2 * hs_gp_XOY(); 

gp_Ax2 * hs_gp_YOZ(); 

gp_Ax2 * hs_gp_ZOX(); 

gp_Pnt2d * hs_gp_Origin2d(); 

gp_Dir2d * hs_gp_DX2d(); 

gp_Dir2d * hs_gp_DY2d(); 

gp_Ax2d * hs_gp_OX2d(); 

gp_Ax2d * hs_gp_OY2d(); 

#ifdef __cplusplus
}
#endif

#endif // HS_GP_H
