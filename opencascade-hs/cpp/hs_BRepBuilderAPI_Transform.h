#ifndef HS_BREPBUILDERAPI_TRANSFORM_H
#define HS_BREPBUILDERAPI_TRANSFORM_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepBuilderAPI_Transform * hs_new_BRepBuilderAPI_Transform_fromShapeTrsfAndCopy(
    TopoDS_Shape * shape, gp_Trsf * trsf, bool copy,
    HSExceptionType* exType,
    void** exPtr
);

void hs_delete_BRepBuilderAPI_Transform(BRepBuilderAPI_Transform * builder);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPBUILDERAPI_TRANSFORM_H
