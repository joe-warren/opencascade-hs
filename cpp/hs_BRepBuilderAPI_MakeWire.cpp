#include <BRepBuilderAPI_MakeWire.hxx>
#include <TopoDS_Wire.hxx>
#include "hs_BRepBuilderAPI_MakeWire.h"

BRepBuilderAPI_MakeWire * hs_new_BRepBuilderAPI_MakeWire(){
    return new BRepBuilderAPI_MakeWire();
}

void hs_delete_BRepBuilderAPI_MakeWire(BRepBuilderAPI_MakeWire* builder){
    delete builder;
}

void hs_BRepBuilderAPI_MakeWire_AddEdge(BRepBuilderAPI_MakeWire* builder, TopoDS_Edge* edge){
    builder->Add(*edge);
}

void hs_BRepBuilderAPI_MakeWire_AddWire(BRepBuilderAPI_MakeWire* builder, TopoDS_Wire* wire){
    builder->Add(*wire);
}

void hs_BRepBuilderAPI_MakeWire_AddListOfShape(BRepBuilderAPI_MakeWire* builder, TopTools_ListOfShape *list){
    builder->Add(*list);
}

TopoDS_Wire * hs_BRepBuilderAPI_MakeWire_Wire(BRepBuilderAPI_MakeWire* builder){
    return new TopoDS_Wire(builder->Wire());
}

bool hs_BRepBuilderAPI_MakeWire_IsDone(BRepBuilderAPI_MakeWire* builder){
    return builder->IsDone();
}

BRepBuilderAPI_WireError hs_BRepBuilderAPI_MakeWire_Error(BRepBuilderAPI_MakeWire* builder){
    return builder->Error();
}
