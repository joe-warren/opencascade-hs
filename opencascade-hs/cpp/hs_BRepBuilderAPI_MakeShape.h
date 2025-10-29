#ifndef HS_BREPBUILDERAPI_MAKE_SHAPE_H
#define HS_BREPBUILDERAPI_MAKE_SHAPE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

TopoDS_Shape * hs_BRepBuilderAPI_MakeShape_shape(BRepBuilderAPI_MakeShape * builder);

void hs_BRepBuilderAPI_MakeShape_build(BRepBuilderAPI_MakeShape* builder);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPBUILDERAPI_MAKE_SHAPE_H
