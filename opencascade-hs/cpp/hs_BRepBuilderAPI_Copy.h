
#ifndef HS_BREPBUILDERAPI_COPY_H
#define HS_BREPBUILDERAPI_COPY_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

TopoDS_Shape * hs_BRepBuilderAPI_Copy_copy(TopoDS_Shape *shape, bool copyGeom, bool copyMesh);

#ifdef __cplusplus
}
#endif

#endif // BREPBUILDERAPI_COPY


