#include <BRepBndLib.hxx>
#include "hs_Exception.h"
#include "hs_BRepBndLib.h"

void hs_BRepBndLib_add(
        TopoDS_Shape * shape, Bnd_Box * box, bool useTriangulation,
        HSExceptionType* exType, void ** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [shape, box, useTriangulation]{
            BRepBndLib::Add(*shape, *box, useTriangulation);
        }
    );
}

void hs_BRepBndLib_addOptimal(
        TopoDS_Shape * shape, Bnd_Box * box, bool useTriangulation, bool useShapeTolerance,
        HSExceptionType* exType, void ** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [shape, box, useTriangulation, useShapeTolerance]{
            BRepBndLib::AddOptimal(*shape, *box, useTriangulation, useShapeTolerance);
        }
    );
}

void hs_BRepBndLib_addOBB(
        TopoDS_Shape *shape, Bnd_OBB * obb, bool isTriangulationUsed, bool isOptimal, bool isShapeToleranceUsed,
        HSExceptionType* exType, void ** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [shape, obb, isTriangulationUsed, isOptimal, isShapeToleranceUsed]{
            BRepBndLib::AddOBB(*shape, *obb, isTriangulationUsed, isOptimal, isShapeToleranceUsed);
        }
    );
}