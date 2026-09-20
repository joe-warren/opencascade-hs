#include <NCollection_IndexedDataMap.hxx>
#include <TCollection_AsciiString.hxx>
#include <TopoDS_Shape.hxx>
#include <TopTools_ShapeMapHasher.hxx>
#include "hs_NCollection_IndexedDataMap.h"

INDEXED_DATA_MAP(TCollection_AsciiString, TCollection_AsciiString, DEFAULT_HASHER(TCollection_AsciiString)) * 
    hs_new_NCollection_IndexedDataMap_TCollection_AsciiString_TCollection_AsciiString(){
        return new NCollection_IndexedDataMap<TCollection_AsciiString, TCollection_AsciiString>();
}

void hs_delete_NCollection_IndexedDataMap_TCollection_AsciiString_TCollection_AsciiString(
        INDEXED_DATA_MAP(TCollection_AsciiString, TCollection_AsciiString, DEFAULT_HASHER(TCollection_AsciiString)) * theMap
    ){
    delete theMap;
}

void hs_delete_NCollection_IndexedDataMap_TopoDS_Shape_NCollection_List_TopoDS_Shape(
    INDEXED_DATA_MAP(TopoDS_Shape, LIST(TopoDS_Shape), TopTools_ShapeMapHasher) * theMap
) {
    delete theMap;
}
