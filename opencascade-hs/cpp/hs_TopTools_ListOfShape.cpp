#include <TopTools_ListOfShape.hxx>
#include <TopoDS_Shape.hxx>
#include <Standard_OutOfRange.hxx>
#include "hs_Exception.h"
#include "hs_TopTools_ListOfShape.h"

TopTools_ListOfShape * hs_new_TopTools_ListOfShape(){
    return new TopTools_ListOfShape();
}

void hs_delete_TopTools_ListOfShape(TopTools_ListOfShape * list){
    delete list;
}

int hs_TopTools_ListOfShape_extent(TopTools_ListOfShape * list){
    return list->Extent();
}

void hs_TopTools_ListOfShape_append(TopTools_ListOfShape * list, TopoDS_Shape * shape){
    list->Append(*shape);
}

// index is 0 based, as the underlying list doesn't support indexed access
TopoDS_Shape * hs_TopTools_ListOfShape_value(
        TopTools_ListOfShape * list, int index,
        HSExceptionType* exType, void ** exPtr
    ){
    return hs_handleEx(exType, exPtr, [list, index]{
        TopTools_ListOfShape::Iterator iterator(*list);
        for(int i = 0; i < index; i++){
            iterator.Next();
        }
        if(!iterator.More()){
            throw Standard_OutOfRange();
        }
        return new TopoDS_Shape(iterator.Value());
    });
}
