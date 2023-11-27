#include <BRepOffsetAPI_MakePipe.hxx>
#include "hs_BRepOffsetAPI_MakePipe.h"

BRepOffsetAPI_MakePipe * hs_new_BRepOffsetAPI_MakePipe_fromWireAndShape(TopoDS_Wire * wire, TopoDS_Shape * profile){
    return new BRepOffsetAPI_MakePipe(*wire, *profile);
}


void hs_delete_BRepOffsetAPI_MakePipe(BRepOffsetAPI_MakePipe * builder){
    delete builder;
}


