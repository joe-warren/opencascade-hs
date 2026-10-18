#ifndef HS_TOPEXP_H
#define HS_TOPEXP_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

void hs_TopExp_mapShapesAndAncestors(
        TopoDS_Shape * shape, TopAbs_ShapeEnum subshapeType, TopAbs_ShapeEnum ancestorType,
        INDEXED_DATA_MAP(TopoDS_Shape, LIST(TopoDS_Shape), TopTools_ShapeMapHasher) * theMap,
        HSExceptionType* exType, void ** exPtr
    );

#ifdef __cplusplus
}
#endif

#endif // HS_TOPEXP_H
