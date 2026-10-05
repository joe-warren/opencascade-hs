#include <NCollection_IndexedDataMap.hxx>
#include <NCollection_List.hxx>
#include <TCollection_AsciiString.hxx>
#include <TopoDS_Shape.hxx>
#include <TopTools_ShapeMapHasher.hxx>
#include "hs_Exception.h"
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

INDEXED_DATA_MAP(TopoDS_Shape, LIST(TopoDS_Shape), TopTools_ShapeMapHasher) *
    hs_new_NCollection_IndexedDataMap_TopoDS_Shape_NCollection_List_TopoDS_Shape(){
        return new NCollection_IndexedDataMap<TopoDS_Shape, NCollection_List<TopoDS_Shape>, TopTools_ShapeMapHasher>();
}

void hs_delete_NCollection_IndexedDataMap_TopoDS_Shape_NCollection_List_TopoDS_Shape(
    INDEXED_DATA_MAP(TopoDS_Shape, LIST(TopoDS_Shape), TopTools_ShapeMapHasher) * theMap
) {
    delete theMap;
}

int hs_NCollection_IndexedDataMap_TopoDS_Shape_NCollection_List_TopoDS_Shape_extent(
    INDEXED_DATA_MAP(TopoDS_Shape, LIST(TopoDS_Shape), TopTools_ShapeMapHasher) * theMap
){
    return theMap->Extent();
}

bool hs_NCollection_IndexedDataMap_TopoDS_Shape_NCollection_List_TopoDS_Shape_contains(
    INDEXED_DATA_MAP(TopoDS_Shape, LIST(TopoDS_Shape), TopTools_ShapeMapHasher) * theMap,
    TopoDS_Shape * shape
){
    return theMap->Contains(*shape);
}

LIST(TopoDS_Shape) * hs_NCollection_IndexedDataMap_TopoDS_Shape_NCollection_List_TopoDS_Shape_findFromKey(
    INDEXED_DATA_MAP(TopoDS_Shape, LIST(TopoDS_Shape), TopTools_ShapeMapHasher) * theMap,
    TopoDS_Shape * shape,
    HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(exType, exPtr, [theMap, shape]{
        return new NCollection_List<TopoDS_Shape>(theMap->FindFromKey(*shape));
    });
}

// index is 1 based
TopoDS_Shape * hs_NCollection_IndexedDataMap_TopoDS_Shape_NCollection_List_TopoDS_Shape_findKey(
    INDEXED_DATA_MAP(TopoDS_Shape, LIST(TopoDS_Shape), TopTools_ShapeMapHasher) * theMap,
    int index,
    HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(exType, exPtr, [theMap, index]{
        return new TopoDS_Shape(theMap->FindKey(index));
    });
}

// index is 1 based
LIST(TopoDS_Shape) * hs_NCollection_IndexedDataMap_TopoDS_Shape_NCollection_List_TopoDS_Shape_findFromIndex(
    INDEXED_DATA_MAP(TopoDS_Shape, LIST(TopoDS_Shape), TopTools_ShapeMapHasher) * theMap,
    int index,
    HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(exType, exPtr, [theMap, index]{
        return new NCollection_List<TopoDS_Shape>(theMap->FindFromIndex(index));
    });
}
