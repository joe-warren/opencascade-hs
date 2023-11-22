#ifndef HS_BREPBUILDERAPI_GTRANSFORM_H
#define HS_BREPBUILDERAPI_GTRANSFORM_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

TopoDS_Shape * hs_BRepBuilderAPI_GTransform_gtransform(TopoDS_Shape * shape, gp_GTrsf * trsf, bool copy);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPBUILDERAPI_GTRANSFORM_H
