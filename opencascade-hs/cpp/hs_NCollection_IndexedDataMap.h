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

void hs_delete_NCollection_IndexedDataMap_TopoDS_Shape_NCollection_List_TopoDS_Shape(
    INDEXED_DATA_MAP(TopoDS_Shape, LIST(TopoDS_Shape), TopTools_ShapeMapHasher) * theMap
);

#ifdef __cplusplus
}
#endif

#endif // HS_NCOLLECTION_INDEXEDDATAMAP_H
