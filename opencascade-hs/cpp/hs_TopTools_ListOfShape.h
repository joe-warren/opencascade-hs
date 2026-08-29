#ifndef HS_TOPTOOLS_LISTOFSHAPE_H
#define HS_TOPTOOLS_LISTOFSHAPE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

TopTools_ListOfShape * hs_new_TopTools_ListOfShape();

void hs_delete_TopTools_ListOfShape(TopTools_ListOfShape * list);

int hs_TopTools_ListOfShape_extent(TopTools_ListOfShape * list);

void hs_TopTools_ListOfShape_append(TopTools_ListOfShape * list, TopoDS_Shape * shape);

TopoDS_Shape * hs_TopTools_ListOfShape_value(
    TopTools_ListOfShape * list, int index,
    HSExceptionType* exType, void ** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_TOPTOOLS_LISTOFSHAPE_H
