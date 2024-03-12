#ifndef HS_SHAPEFIX_SOLID_H
#define HS_SHAPEFIX_SOLID_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

ShapeFix_Solid * hs_new_ShapeFix_Solid_fromSolid(TopoDS_Solid * solid);

void hs_delete_ShapeFix_Solid(ShapeFix_Solid * shapeFix);

bool hs_ShapeFix_Solid_perform(ShapeFix_Solid * shapeFix, Message_ProgressRange * progress);

TopoDS_Shape * hs_ShapeFix_Solid_solid(ShapeFix_Solid * shapeFix);

bool hs_ShapeFix_Solid_status(ShapeFix_Solid * shapeFix, ShapeExtend_Status status);

#ifdef __cplusplus
}
#endif

#endif // HS_SHAPEFIX_SOLID_H
