#ifndef HS_BREPBUILDERAPI_GTRANSFORM_H
#define HS_BREPBUILDERAPI_GTRANSFORM_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepBuilderAPI_GTransform * hs_new_BRepBuilderAPI_GTransform_fromShapeGTrsfAndCopy(
    TopoDS_Shape * shape, gp_GTrsf * trsf, bool copy,
    HSExceptionType* exType,
    void** exPtr
);

void hs_delete_BRepBuilderAPI_GTransform(BRepBuilderAPI_GTransform * builder);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPBUILDERAPI_GTRANSFORM_H
