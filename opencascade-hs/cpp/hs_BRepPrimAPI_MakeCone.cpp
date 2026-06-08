#include <BRepPrimAPI_MakeCone.hxx>
#include "hs_Exception.h"
#include "hs_BRepPrimAPI_MakeCone.h"

TopoDS_Solid * hs_BRepPrimAPI_MakeCone_fromTwoRadiiAndHeight(
        double r1, double r2, double h,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [r1, r2, h]{
        auto builder = BRepPrimAPI_MakeCone(r1, r2, h);
        return new TopoDS_Solid(builder.Solid());
    });
}
