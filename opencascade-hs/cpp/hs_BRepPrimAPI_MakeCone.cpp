#include <BRepPrimAPI_MakeCone.hxx>
#include "hs_BRepPrimAPI_MakeCone.h"

TopoDS_Solid * hs_BRepPrimAPI_MakeCone_fromTwoRadiiAndHeight(double r1, double r2, double h){
    auto builder = BRepPrimAPI_MakeCone(r1, r2, h);
    return new TopoDS_Solid(builder.Solid());
}
