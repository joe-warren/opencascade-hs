#ifndef HS_BREPBUILDERAPI_MAKE_SHAPE_H
#define HS_BREPBUILDERAPI_MAKE_SHAPE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

TopoDS_Shape * hs_BRepBuilderAPI_MakeShape_shape(
    BRepBuilderAPI_MakeShape * builder,
    HSExceptionType* exType,
    void** exPtr
);

void hs_BRepBuilderAPI_MakeShape_build(
        BRepBuilderAPI_MakeShape* builder,
        HSExceptionType* exType,
        void** exPtr
);

TopTools_ListOfShape * hs_BRepBuilderAPI_MakeShape_modified(
    BRepBuilderAPI_MakeShape * builder,
    TopoDS_Shape * shape,
    HSExceptionType* exType,
    void** exPtr
);

TopTools_ListOfShape * hs_BRepBuilderAPI_MakeShape_generated(
    BRepBuilderAPI_MakeShape * builder,
    TopoDS_Shape * shape,
    HSExceptionType* exType,
    void** exPtr
);

bool hs_BRepBuilderAPI_MakeShape_isDeleted(
    BRepBuilderAPI_MakeShape * builder,
    TopoDS_Shape * shape,
    HSExceptionType* exType,
    void** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPBUILDERAPI_MAKE_SHAPE_H
