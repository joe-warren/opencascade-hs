#include <BRepLib.hxx>
#include "hs_BRepLib.h"

bool hs_BRepLib_orientClosedSolid(TopoDS_Solid * solid){
    return BRepLib::OrientClosedSolid(*solid);
}