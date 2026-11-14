#include <BRepLib.hxx>
#include "hs_Exception.h"
#include "hs_BRepLib.h"

bool hs_BRepLib_orientClosedSolid(
        TopoDS_Solid * solid,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleExWithDefault(
        exType,
        exPtr,
        [solid]{
            return BRepLib::OrientClosedSolid(*solid);
        },
        false
    );
}

bool hs_BRepLib_buildCurve3d(
        TopoDS_Edge* edge, double tolerance, GeomAbs_Shape continuity, int maxDegree, int maxSegment,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleExWithDefault(
        exType,
        exPtr,
        [edge, tolerance, continuity, maxDegree, maxSegment]{
            return BRepLib::BuildCurve3d(*edge, tolerance, continuity, maxDegree, maxSegment);
        },
        false
    );
}

void hs_BRepLib_encodeRegularity(
    TopoDS_Shape * shape, double angularTolerance,
    HSExceptionType* exType, void ** exPtr
){
    hs_handleExVoid(exType, exPtr,
        [shape, angularTolerance]{
            BRepLib::EncodeRegularity(*shape, angularTolerance);
        }
    );
}