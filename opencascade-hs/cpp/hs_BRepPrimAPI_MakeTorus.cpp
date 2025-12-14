#include <BRepPrimAPI_MakeTorus.hxx>
#include <TopoDS_Solid.hxx>
#include <TopoDS_Shell.hxx>
#include "hs_BRepPrimAPI_MakeTorus.h"

BRepPrimAPI_MakeTorus * hs_new_BRepPrimAPI_MakeTorus_fromRadii(double major, double minor){
    return new BRepPrimAPI_MakeTorus(major, minor);
}

void hs_delete_BRepPrimAPI_MakeTorus(BRepPrimAPI_MakeTorus * makeTorus){
    delete makeTorus;
}