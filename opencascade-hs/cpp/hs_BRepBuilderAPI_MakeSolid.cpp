#include <BRepBuilderAPI_MakeSolid.hxx>
#include <TopoDS_Solid.hxx>
#include "hs_Exception.h"
#include "hs_BRepBuilderAPI_MakeSolid.h"

BRepBuilderAPI_MakeSolid * hs_new_BRepBuilderAPI_MakeSolid(){
    auto everywhere = BRepBuilderAPI_MakeSolid().Shape();
    return new BRepBuilderAPI_MakeSolid();
}

void hs_delete_BRepBuilderAPI_MakeSolid(BRepBuilderAPI_MakeSolid * builder){
    delete builder;
}

void hs_BRepBuilderAPI_MakeSolid_add(
        BRepBuilderAPI_MakeSolid * builder, TopoDS_Shell * shell,
        HSExceptionType* exType,
        void** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [builder, shell]{
        builder->Add(*shell);
    });
}

TopoDS_Solid * hs_BRepBuilderAPI_MakeSolid_solid(
        BRepBuilderAPI_MakeSolid * builder,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [builder]{
        return new TopoDS_Solid(builder->Solid());
    });
}