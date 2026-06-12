#include <BRepPrimAPI_MakeSphere.hxx>
#include "hs_Exception.h"
#include "hs_BRepPrimAPI_MakeSphere.h"


TopoDS_Solid * hs_BRepPrimAPI_MakeSphere_fromRadius(
        double r,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [r]{
        auto builder = BRepPrimAPI_MakeSphere(r);
        return new TopoDS_Solid(builder.Solid());
    });
}

TopoDS_Solid * hs_BRepPrimAPI_MakeSphere_fromPntAndRadius(
        gp_Pnt * center, double r,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [center, r]{
        auto builder = BRepPrimAPI_MakeSphere(*center, r);
        return new TopoDS_Solid(builder.Solid());
    });
}