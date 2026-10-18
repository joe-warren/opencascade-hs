#include <NCollection_List.hxx>
#include <TopoDS_Shape.hxx>
#include "hs_NCollection_List.h"

LIST(TopoDS_Shape) * hs_new_NCollection_List_TopoDS_Shape(){
    return new NCollection_List<TopoDS_Shape>();
}

void hs_delete_NCollection_List_TopoDS_Shape(LIST(TopoDS_Shape) * list){
    delete list;
}

int hs_NCollection_List_TopoDS_Shape_extent(LIST(TopoDS_Shape) * list){
    return list->Extent();
}

void hs_NCollection_List_TopoDS_Shape_append(LIST(TopoDS_Shape) * list, TopoDS_Shape * shape){
    list->Append(*shape);
}

LIST_ITERATOR(TopoDS_Shape) * hs_new_NCollection_List_TopoDS_Shape_Iterator(LIST(TopoDS_Shape) * list){
    return new NCollection_List<TopoDS_Shape>::Iterator(*list);
}

void hs_delete_NCollection_List_TopoDS_Shape_Iterator(LIST_ITERATOR(TopoDS_Shape) * iterator){
    delete iterator;
}

bool hs_NCollection_List_TopoDS_Shape_Iterator_more(LIST_ITERATOR(TopoDS_Shape) * iterator){
    return iterator->More();
}

void hs_NCollection_List_TopoDS_Shape_Iterator_next(LIST_ITERATOR(TopoDS_Shape) * iterator){
    iterator->Next();
}

TopoDS_Shape * hs_NCollection_List_TopoDS_Shape_Iterator_value(LIST_ITERATOR(TopoDS_Shape) * iterator){
    return new TopoDS_Shape(iterator->Value());
}
