#include <BRepPrimAPI_MakeCylinder.hxx>
#include "hs_Exception.h"
#include "hs_BRepPrimAPI_MakeCylinder.h"

TopoDS_Solid * hs_BRepPrimAPI_MakeCylinder_fromRadiusAndHeight(
        double r, double h,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [r, h]{
        auto builder = BRepPrimAPI_MakeCylinder(r, h);
        return new TopoDS_Solid(builder.Solid());
    });
}
