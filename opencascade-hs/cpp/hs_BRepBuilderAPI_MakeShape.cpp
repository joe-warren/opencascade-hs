#include <BRepBuilderAPI_MakeShape.hxx>
#include <TopoDS_Shape.hxx>
#include <TopTools_ListOfShape.hxx>
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
TopTools_ListOfShape * hs_BRepBuilderAPI_MakeShape_modified(
        BRepBuilderAPI_MakeShape * builder,
        TopoDS_Shape * shape,
        HSExceptionType* exType,
        void** exPtr
    ){
    return hs_handleEx(
        exType,
        exPtr,
        [builder, shape]{
            return new TopTools_ListOfShape(builder->Modified(*shape));
        }
    );
}

TopTools_ListOfShape * hs_BRepBuilderAPI_MakeShape_generated(
        BRepBuilderAPI_MakeShape * builder,
        TopoDS_Shape * shape,
        HSExceptionType* exType,
        void** exPtr
    ){
    return hs_handleEx(
        exType,
        exPtr,
        [builder, shape]{
            return new TopTools_ListOfShape(builder->Generated(*shape));
        }
    );
}

bool hs_BRepBuilderAPI_MakeShape_isDeleted(
        BRepBuilderAPI_MakeShape * builder,
        TopoDS_Shape * shape,
        HSExceptionType* exType,
        void** exPtr
    ){
    return hs_handleExWithDefault(
        exType,
        exPtr,
        [builder, shape]{
            return builder->IsDeleted(*shape) == Standard_True;
        },
        false
    );
}
