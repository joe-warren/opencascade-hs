#include <TopoDS_Shape.hxx>
#include "hs_TopoDS_Shape.h"

TopoDS_Shape * hs_new_TopoDS_Shape(){
    return new TopoDS_Shape();
}

void hs_delete_TopoDS_Shape(TopoDS_Shape * shape){
    delete shape;
}

bool hs_TopoDS_Shape_IsNull(TopoDS_Shape * shape){
    return shape->IsNull();
}

void hs_TopoDS_Shape_Nullify(TopoDS_Shape * shape){
    shape->Nullify();
}

TopLoc_Location * hs_TopoDS_Shape_Location(TopoDS_Shape * shape){
    return new TopLoc_Location(shape->Location());
}

void hs_TopoDS_Shape_SetLocation(TopoDS_Shape * shape, TopLoc_Location * location){
    shape->Location(*location);
}

TopoDS_Shape * hs_TopoDS_Shape_Located(TopoDS_Shape * shape, TopLoc_Location * location){
    return new TopoDS_Shape(shape->Located(*location));
}

TopAbs_Orientation hs_TopoDS_Shape_Orientation(TopoDS_Shape * shape){
    return shape->Orientation();
}

void hs_TopoDS_Shape_SetOrientation(TopoDS_Shape * shape, TopAbs_Orientation orientation){
    return shape->Orientation(orientation);
}

TopoDS_Shape * hs_TopoDS_Shape_Oriented(TopoDS_Shape * shape, TopAbs_Orientation orientation){
    return new TopoDS_Shape(shape->Oriented(orientation));
}

TopAbs_ShapeEnum hs_TopoDS_Shape_ShapeType(TopoDS_Shape * shape){
    return shape->ShapeType();
}

bool hs_TopoDS_Shape_Free(TopoDS_Shape * shape){
    return shape->Free();
}

void hs_TopoDS_Shape_SetFree(TopoDS_Shape * shape, bool b){
    shape->Free(b);
}


bool hs_TopoDS_Shape_Locked(TopoDS_Shape * shape){
    return shape->Locked();
}

void hs_TopoDS_Shape_SetLocked(TopoDS_Shape * shape, bool b){
    shape->Locked(b);
}

bool hs_TopoDS_Shape_Modified(TopoDS_Shape * shape){
    return shape->Modified();
}

void hs_TopoDS_Shape_SetModified(TopoDS_Shape * shape, bool b){
    shape->Modified(b);
}


bool hs_TopoDS_Shape_Checked(TopoDS_Shape * shape){
    return shape->Checked();
}

void hs_TopoDS_Shape_SetChecked(TopoDS_Shape * shape, bool b){
    shape->Checked(b);
}


bool hs_TopoDS_Shape_Orientable(TopoDS_Shape * shape){
    return shape->Orientable();
}

void hs_TopoDS_Shape_SetOrientable(TopoDS_Shape * shape, bool b){
    shape->Orientable(b);
}

bool hs_TopoDS_Shape_Closed(TopoDS_Shape * shape){
    return shape->Closed();
}

void hs_TopoDS_Shape_SetClosed(TopoDS_Shape * shape, bool b){
    shape->Closed(b);
}


bool hs_TopoDS_Shape_Infinite(TopoDS_Shape * shape){
    return shape->Infinite();
}

void hs_TopoDS_Shape_SetInfinite(TopoDS_Shape * shape, bool b){
    shape->Infinite(b);
}


bool hs_TopoDS_Shape_Convex(TopoDS_Shape * shape){
    return shape->Convex();
}

void hs_TopoDS_Shape_SetConvex(TopoDS_Shape * shape, bool b){
    shape->Convex(b);
}

void hs_TopoDS_Shape_Move(TopoDS_Shape * shape, TopLoc_Location * position){
    shape->Move(*position);
}

TopoDS_Shape * hs_TopoDS_Shape_Moved(TopoDS_Shape * shape, TopLoc_Location * position){
    return new TopoDS_Shape(shape->Moved(*position));
}

int hs_TopoDS_Shape_NbChildren(TopoDS_Shape * shape){
    return shape->NbChildren();
}

void hs_TopoDS_Shape_Reverse(TopoDS_Shape * shape){
    shape->Reverse();
}

TopoDS_Shape * hs_TopoDS_Shape_Reversed(TopoDS_Shape * shape){
    return new TopoDS_Shape(shape->Reversed());
}


void hs_TopoDS_Shape_Complement(TopoDS_Shape * shape){
    shape->Complement();
}

TopoDS_Shape * hs_TopoDS_Shape_Complemented(TopoDS_Shape * shape){
    return new TopoDS_Shape(shape->Complemented());
}

void hs_TopoDS_Shape_Compose(TopoDS_Shape * shape, TopAbs_Orientation orient){
    shape->Compose(orient);
}

TopoDS_Shape * hs_TopoDS_Shape_Composed(TopoDS_Shape * shape, TopAbs_Orientation orient){
    return new TopoDS_Shape(shape->Composed(orient));
}

bool hs_TopoDS_Shape_IsEqual(TopoDS_Shape * a, TopoDS_Shape* b){
    return a->IsEqual(*b);
}

bool hs_TopoDS_Shape_IsSame(TopoDS_Shape * a, TopoDS_Shape* b){
    return a->IsSame(*b);
}

bool hs_TopoDS_Shape_IsPartner(TopoDS_Shape * a, TopoDS_Shape* b){
    return a->IsPartner(*b);
}

bool hs_TopoDS_Shape_IsNotEqual(TopoDS_Shape * a, TopoDS_Shape* b){
    return a->IsNotEqual(*b);
}


void hs_TopoDS_Shape_EmptyCopy(TopoDS_Shape * shape){
    shape->EmptyCopy();
}

TopoDS_Shape * hs_TopoDS_Shape_EmptyCopied(TopoDS_Shape * shape){
    return new TopoDS_Shape(shape->EmptyCopied());
}
