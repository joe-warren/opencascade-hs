#include <BRepPrimAPI_MakeBox.hxx>
#include <TopoDS_Solid.hxx>
#include <TopoDS_Shell.hxx>
#include "hs_BRepPrimAPI_MakeBox.h"


BRepPrimAPI_MakeBox * hs_BRepPrimAPI_MakeBox_fromPnts(gp_Pnt *a, gp_Pnt *b){
    return new BRepPrimAPI_MakeBox(*a, *b);
}

void hs_delete_BRepPrimAPI_MakeBox(BRepPrimAPI_MakeBox* builder){
    delete builder;
}

TopoDS_Shape * hs_BRepPrimAPI_MakeBox_Solid(BRepPrimAPI_MakeBox* builder){
    return new TopoDS_Shape(builder->Solid());
}

TopoDS_Shell * hs_BRepPrimAPI_MakeBox_Shell(BRepPrimAPI_MakeBox* builder){
    return new TopoDS_Shell(builder->Shell());
}
