#ifndef HS_TOPTOOLS_INDEXEDDATAMAPOFSHAPELISTOFSHAPE_H
#define HS_TOPTOOLS_INDEXEDDATAMAPOFSHAPELISTOFSHAPE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

void hs_delete_TopTools_IndexedDataMapOfShapeListOfShape(TopTools_IndexedDataMapOfShapeListOfShape * map);

int hs_TopTools_IndexedDataMapOfShapeListOfShape_extent(TopTools_IndexedDataMapOfShapeListOfShape * map);

bool hs_TopTools_IndexedDataMapOfShapeListOfShape_contains(TopTools_IndexedDataMapOfShapeListOfShape * map, TopoDS_Shape * shape);

TopTools_ListOfShape * hs_TopTools_IndexedDataMapOfShapeListOfShape_findFromKey(
    TopTools_IndexedDataMapOfShapeListOfShape * map, TopoDS_Shape * shape,
    HSExceptionType* exType, void ** exPtr
);

TopoDS_Shape * hs_TopTools_IndexedDataMapOfShapeListOfShape_findKey(
    TopTools_IndexedDataMapOfShapeListOfShape * map, int index,
    HSExceptionType* exType, void ** exPtr
);

TopTools_ListOfShape * hs_TopTools_IndexedDataMapOfShapeListOfShape_findFromIndex(
    TopTools_IndexedDataMapOfShapeListOfShape * map, int index,
    HSExceptionType* exType, void ** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_TOPTOOLS_INDEXEDDATAMAPOFSHAPELISTOFSHAPE_H
