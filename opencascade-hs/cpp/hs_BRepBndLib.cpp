#include <BRepBndLib.hxx>
#include "hs_BRepBndLib.h"

void hs_BRepBndLib_add(TopoDS_Shape * shape, Bnd_Box * box, bool useTriangulation){
    BRepBndLib::Add(*shape, *box, useTriangulation);
}