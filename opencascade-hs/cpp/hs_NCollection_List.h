#ifndef HS_NCOLLECTION_LIST_H
#define HS_NCOLLECTION_LIST_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

LIST(TopoDS_Shape) * hs_new_NCollection_List_TopoDS_Shape();

void hs_delete_NCollection_List_TopoDS_Shape(LIST(TopoDS_Shape) * list);

int hs_NCollection_List_TopoDS_Shape_extent(LIST(TopoDS_Shape) * list);

void hs_NCollection_List_TopoDS_Shape_append(LIST(TopoDS_Shape) * list, TopoDS_Shape * shape);

TopoDS_Shape * hs_NCollection_List_TopoDS_Shape_value(
    LIST(TopoDS_Shape) * list, int index,
    HSExceptionType* exType, void ** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_NCOLLECTION_LIST_H
