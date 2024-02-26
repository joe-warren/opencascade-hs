#include <BRepBndLib.hxx>
#include "hs_BRepBndLib.h"

void hs_BRepBndLib_add(TopoDS_Shape * shape, Bnd_Box * box, bool useTriangulation){
    BRepBndLib::Add(*shape, *box, useTriangulation);
}

void hs_BRepBndLib_addOptimal(TopoDS_Shape * shape, Bnd_Box * box, bool useTriangulation, bool useShapeTolerance){
    BRepBndLib::AddOptimal(*shape, *box, useTriangulation, useShapeTolerance);
}

void hs_BRepBndLib_addOBB(TopoDS_Shape *shape, Bnd_OBB * obb, bool isTriangulationUsed, bool isOptimal, bool isShapeToleranceUsed){
    BRepBndLib::AddOBB(*shape, *obb, isTriangulationUsed, isOptimal, isShapeToleranceUsed);
}