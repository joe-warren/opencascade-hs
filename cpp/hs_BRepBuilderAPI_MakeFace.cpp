#include <BRepBuilderAPI_MakeFace.hxx>
#include <TopoDS_Face.hxx>
#include "hs_BRepBuilderAPI_MakeFace.h"

BRepBuilderAPI_MakeFace * hs_new_BRepBuilderAPI_MakeFace(){
    return new BRepBuilderAPI_MakeFace();
}

void hs_delete_BRepBuilderAPI_MakeFace(BRepBuilderAPI_MakeFace * builder){
    delete builder;
}

BRepBuilderAPI_MakeFace * hs_new_BRepBuilderAPI_MakeFace_fromFace(TopoDS_Face * face){
    return new BRepBuilderAPI_MakeFace(*face);
}

BRepBuilderAPI_MakeFace * hs_new_BRepBuilderAPI_MakeFace_fromSurface(Handle(Geom_Surface)* surface, double tolerance){
    return new BRepBuilderAPI_MakeFace(*surface, tolerance);
}

BRepBuilderAPI_MakeFace * hs_new_BRepBuilderAPI_MakeFace_fromSurfaceAndBounds(Handle(Geom_Surface)* surface, double uMin, double uMax, double vMin, double vMax, double tolerance){
    return new BRepBuilderAPI_MakeFace(*surface, uMin, uMax, vMin, vMax, tolerance);
}

BRepBuilderAPI_MakeFace * hs_new_BRepBuilderAPI_MakeFace_fromSurfaceAndWire(Handle(Geom_Surface)* surface, TopoDS_Wire* wire, bool inside){
    return new BRepBuilderAPI_MakeFace(*surface, *wire, inside);
}

BRepBuilderAPI_MakeFace * hs_new_BRepBuilderAPI_MakeFace_fromWire(TopoDS_Wire* wire, bool onlyPlane){
    return new BRepBuilderAPI_MakeFace(*wire, onlyPlane);
}

void hs_BRepBuilderAPI_MakeFace_Add(BRepBuilderAPI_MakeFace* builder, TopoDS_Wire* wire){
    builder->Add(*wire);
}

bool hs_BRepBuilderAPI_MakeFace_IsDone(BRepBuilderAPI_MakeFace* builder){
    return builder->IsDone();
}

BRepBuilderAPI_FaceError hs_BRepBuilderAPI_MakeFace_Error(BRepBuilderAPI_MakeFace * builder){
    return builder->Error();
}

TopoDS_Face * hs_BRepBuilderAPI_MakeFace_Face(BRepBuilderAPI_MakeFace * builder){
    return new TopoDS_Face(builder->Face());
}
