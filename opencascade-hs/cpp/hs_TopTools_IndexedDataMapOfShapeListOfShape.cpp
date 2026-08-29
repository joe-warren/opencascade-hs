#include <TopTools_IndexedDataMapOfShapeListOfShape.hxx>
#include <TopTools_ListOfShape.hxx>
#include <TopoDS_Shape.hxx>
#include "hs_Exception.h"
#include "hs_TopTools_IndexedDataMapOfShapeListOfShape.h"

void hs_delete_TopTools_IndexedDataMapOfShapeListOfShape(TopTools_IndexedDataMapOfShapeListOfShape * map){
    delete map;
}

int hs_TopTools_IndexedDataMapOfShapeListOfShape_extent(TopTools_IndexedDataMapOfShapeListOfShape * map){
    return map->Extent();
}

bool hs_TopTools_IndexedDataMapOfShapeListOfShape_contains(TopTools_IndexedDataMapOfShapeListOfShape * map, TopoDS_Shape * shape){
    return map->Contains(*shape);
}

TopTools_ListOfShape * hs_TopTools_IndexedDataMapOfShapeListOfShape_findFromKey(
        TopTools_IndexedDataMapOfShapeListOfShape * map, TopoDS_Shape * shape,
        HSExceptionType* exType, void ** exPtr
    ){
    return hs_handleEx(exType, exPtr, [map, shape]{
        return new TopTools_ListOfShape(map->FindFromKey(*shape));
    });
}

TopoDS_Shape * hs_TopTools_IndexedDataMapOfShapeListOfShape_findKey(
        TopTools_IndexedDataMapOfShapeListOfShape * map, int index,
        HSExceptionType* exType, void ** exPtr
    ){
    return hs_handleEx(exType, exPtr, [map, index]{
        return new TopoDS_Shape(map->FindKey(index));
    });
}

TopTools_ListOfShape * hs_TopTools_IndexedDataMapOfShapeListOfShape_findFromIndex(
        TopTools_IndexedDataMapOfShapeListOfShape * map, int index,
        HSExceptionType* exType, void ** exPtr
    ){
    return hs_handleEx(exType, exPtr, [map, index]{
        return new TopTools_ListOfShape(map->FindFromIndex(index));
    });
}
