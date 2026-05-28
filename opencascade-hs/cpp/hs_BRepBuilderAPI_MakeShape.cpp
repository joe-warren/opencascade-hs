#include <BRepBuilderAPI_MakeShape.hxx>
#include <TopoDS_Shape.hxx>
#include "hs_Exception.h"
#include "hs_BRepBuilderAPI_MakeShape.h"

TopoDS_Shape * hs_BRepBuilderAPI_MakeShape_shape(
        BRepBuilderAPI_MakeShape* builder,
        HSExceptionType* exType,
        void** exPtr

    ){
        
    return hs_handleEx(
        exType, 
        exPtr,
        [builder]{ 
            return new TopoDS_Shape(
                builder->Shape()
            );
        }
    );
}

void hs_BRepBuilderAPI_MakeShape_build(
        BRepBuilderAPI_MakeShape* builder, 
        HSExceptionType* exType,
        void** exPtr
    ){
    hs_handleExVoid(
        exType,
        exPtr,
        [builder]{
            builder->Build();
        }
    );
}