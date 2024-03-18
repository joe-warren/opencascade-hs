#ifndef HS_TOPODS_BUILDER_H
#define HS_TOPODS_BUILDER_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

TopoDS_Builder * hs_new_TopoDS_Builder(void);

void hs_delete_TopoDS_Builder(TopoDS_Builder * builder);

void hs_TopoDS_Builder_makeWire(TopoDS_Builder * builder, TopoDS_Wire * wire);

void hs_TopoDS_Builder_makeShell(TopoDS_Builder * builder, TopoDS_Shell * shell);

void hs_TopoDS_Builder_makeSolid(TopoDS_Builder * builder, TopoDS_Solid * solid);

void hs_TopoDS_Builder_makeCompSolid(TopoDS_Builder * builder, TopoDS_CompSolid * solid);

void hs_TopoDS_Builder_makeCompound(TopoDS_Builder * builder, TopoDS_Compound * compound);

void hs_TopoDS_Builder_add(TopoDS_Builder * builder, TopoDS_Shape * s, TopoDS_Shape * c);

void hs_TopoDS_Builder_remove(TopoDS_Builder * builder, TopoDS_Shape * s, TopoDS_Shape * c);

#ifdef __cplusplus
}
#endif

#endif // HS_TOPODS_BUILDER_H


