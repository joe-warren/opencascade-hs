#ifndef HS_NCOLLECTION_INDEXEDDATAMAP_H
#define HS_NCOLLECTION_INDEXEDDATAMAP_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

INDEXED_DATA_MAP(TCollection_AsciiString, TCollection_AsciiString, DEFAULT_HASHER(TCollection_AsciiString)) *
    hs_new_NCollection_IndexedDataMap_TCollection_AsciiString_TCollection_AsciiString();

void hs_delete_NCollection_IndexedDataMap_TCollection_AsciiString_TCollection_AsciiString(
    INDEXED_DATA_MAP(TCollection_AsciiString, TCollection_AsciiString, DEFAULT_HASHER(TCollection_AsciiString)) *theMap
);

INDEXED_DATA_MAP(TopoDS_Shape, LIST(TopoDS_Shape), TopTools_ShapeMapHasher) *
    hs_new_NCollection_IndexedDataMap_TopoDS_Shape_NCollection_List_TopoDS_Shape();

void hs_delete_NCollection_IndexedDataMap_TopoDS_Shape_NCollection_List_TopoDS_Shape(
    INDEXED_DATA_MAP(TopoDS_Shape, LIST(TopoDS_Shape), TopTools_ShapeMapHasher) * theMap
);

int hs_NCollection_IndexedDataMap_TopoDS_Shape_NCollection_List_TopoDS_Shape_extent(
    INDEXED_DATA_MAP(TopoDS_Shape, LIST(TopoDS_Shape), TopTools_ShapeMapHasher) * theMap
);

bool hs_NCollection_IndexedDataMap_TopoDS_Shape_NCollection_List_TopoDS_Shape_contains(
    INDEXED_DATA_MAP(TopoDS_Shape, LIST(TopoDS_Shape), TopTools_ShapeMapHasher) * theMap,
    TopoDS_Shape * shape
);

LIST(TopoDS_Shape) * hs_NCollection_IndexedDataMap_TopoDS_Shape_NCollection_List_TopoDS_Shape_findFromKey(
    INDEXED_DATA_MAP(TopoDS_Shape, LIST(TopoDS_Shape), TopTools_ShapeMapHasher) * theMap,
    TopoDS_Shape * shape,
    HSExceptionType* exType, void ** exPtr
);

TopoDS_Shape * hs_NCollection_IndexedDataMap_TopoDS_Shape_NCollection_List_TopoDS_Shape_findKey(
    INDEXED_DATA_MAP(TopoDS_Shape, LIST(TopoDS_Shape), TopTools_ShapeMapHasher) * theMap,
    int index,
    HSExceptionType* exType, void ** exPtr
);

LIST(TopoDS_Shape) * hs_NCollection_IndexedDataMap_TopoDS_Shape_NCollection_List_TopoDS_Shape_findFromIndex(
    INDEXED_DATA_MAP(TopoDS_Shape, LIST(TopoDS_Shape), TopTools_ShapeMapHasher) * theMap,
    int index,
    HSExceptionType* exType, void ** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_NCOLLECTION_INDEXEDDATAMAP_H
