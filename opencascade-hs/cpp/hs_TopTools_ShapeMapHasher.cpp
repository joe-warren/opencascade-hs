#include <TopTools_ShapeMapHasher.hxx>
#include "hs_TopTools_ShapeMapHasher.h"

int hs_TopTools_ShapeMapHasher_hash(TopoDS_Shape * shape){
    auto hasher = TopTools_ShapeMapHasher();
    return hasher(*shape);
}

bool hs_TopTools_ShapeMapHasher_isEqual(TopoDS_Shape * shapeA, TopoDS_Shape * shapeB){
    auto hasher = TopTools_ShapeMapHasher();
    return hasher(*shapeA, *shapeB);
}