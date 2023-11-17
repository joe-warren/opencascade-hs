#include <BRepBuilderAPI_MakeSolid.hxx>
#include <TopoDS_Solid.hxx>
#include "hs_BRepBuilderAPI_MakeSolid.h"

BRepBuilderAPI_MakeSolid * hs_new_BRepBuilderAPI_MakeSolid(){
    return new BRepBuilderAPI_MakeSolid();
}

void hs_delete_BRepBuilderAPI_MakeSolid(BRepBuilderAPI_MakeSolid * builder){
    delete builder;
}

void hs_BRepBuilderAPI_MakeSolid_add(BRepBuilderAPI_MakeSolid * builder, TopoDS_Shell * shell){
    builder->Add(*shell);
}

TopoDS_Solid * hs_BRepBuilderAPI_MakeSolid_solid(BRepBuilderAPI_MakeSolid * builder){
    return new TopoDS_Solid(builder->Solid());
}