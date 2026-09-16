#include <NCollection_List.hxx>
#include <TopoDS_Shape.hxx>
#include "hs_NCollection_List.h"

void hs_delete_NCollection_List_TopoDS_Shape(LIST(TopoDS_Shape) * list){
    delete list;
}