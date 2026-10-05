#include <NCollection_List.hxx>
#include <TopoDS_Shape.hxx>
#include <Standard_OutOfRange.hxx>
#include "hs_Exception.h"
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

// index is 0 based, as the underlying list doesn't support indexed access
TopoDS_Shape * hs_NCollection_List_TopoDS_Shape_value(
        LIST(TopoDS_Shape) * list, int index,
        HSExceptionType* exType, void ** exPtr
    ){
    return hs_handleEx(exType, exPtr, [list, index]{
        NCollection_List<TopoDS_Shape>::Iterator iterator(*list);
        for(int i = 0; i < index && iterator.More(); i++){
            iterator.Next();
        }
        if(!iterator.More()){
            throw Standard_OutOfRange();
        }
        return new TopoDS_Shape(iterator.Value());
    });
}
