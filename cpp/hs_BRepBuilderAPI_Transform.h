#ifndef HS_BREPBUILDERAPI_TRANSFORM_H
#define HS_BREPBUILDERAPI_TRANSFORM_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

TopoDS_Shape * hs_BRepBuilderAPI_Transform_transform(TopoDS_Shape * shape, gp_Trsf * trsf, bool copy);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPBUILDERAPI_TRANSFORM_H
