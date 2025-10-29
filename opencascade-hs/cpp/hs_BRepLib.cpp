#include <BRepLib.hxx>
#include "hs_BRepLib.h"

bool hs_BRepLib_orientClosedSolid(TopoDS_Solid * solid){
    return BRepLib::OrientClosedSolid(*solid);
}

bool hs_BRepLib_buildCurve3d(TopoDS_Edge* edge, double tolerance, GeomAbs_Shape continuity, int maxDegree, int maxSegment){
    return BRepLib::BuildCurve3d(*edge, tolerance, continuity, maxDegree, maxSegment);
}