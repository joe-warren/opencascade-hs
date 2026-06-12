#include <BRepPrimAPI_MakePrism.hxx>
#include "hs_Exception.h"
#include "hs_BRepPrimAPI_MakePrism.h"


TopoDS_Shape * hs_BRepPrimAPI_MakePrism_fromVec(TopoDS_Shape * shape, gp_Vec * vec, bool copy, bool canonize,
        HSExceptionType* exType,
        void** exPtr
    ){
        
    return hs_handleEx(
        exType,
        exPtr,
        [shape, vec, copy, canonize]{
            auto builder = BRepPrimAPI_MakePrism(*shape, *vec, copy, canonize);
            return new TopoDS_Shape(builder.Shape());
    });
}

TopoDS_Shape * hs_BRepPrimAPI_MakePrism_fromDir(TopoDS_Shape * shape, gp_Dir * dir, bool inf, bool copy, bool canonize,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [shape, dir, inf, copy, canonize]{
            auto builder = BRepPrimAPI_MakePrism(*shape, *dir, inf, copy, canonize);
            return new TopoDS_Shape(builder.Shape());
    });
}
