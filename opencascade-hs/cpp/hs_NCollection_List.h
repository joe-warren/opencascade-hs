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

LIST_ITERATOR(TopoDS_Shape) * hs_new_NCollection_List_TopoDS_Shape_Iterator(LIST(TopoDS_Shape) * list);

void hs_delete_NCollection_List_TopoDS_Shape_Iterator(LIST_ITERATOR(TopoDS_Shape) * iterator);

bool hs_NCollection_List_TopoDS_Shape_Iterator_more(LIST_ITERATOR(TopoDS_Shape) * iterator);

void hs_NCollection_List_TopoDS_Shape_Iterator_next(LIST_ITERATOR(TopoDS_Shape) * iterator);

TopoDS_Shape * hs_NCollection_List_TopoDS_Shape_Iterator_value(LIST_ITERATOR(TopoDS_Shape) * iterator);

#ifdef __cplusplus
}
#endif

#endif // HS_NCOLLECTION_LIST_H
