#include <BOPAlgo_Builder.hxx>
#include <TopTools_ListOfShape.hxx>
#include <TopoDS_Shape.hxx>
#include "hs_Exception.h"
#include "hs_BOPAlgo_Builder.h"

BOPAlgo_Builder * hs_new_BOPAlgo_Builder(){
    return new BOPAlgo_Builder();
}

void hs_delete_BOPAlgo_Builder(BOPAlgo_Builder * builder){
    delete builder;
}

void hs_BOPAlgo_Builder_AddArgument(
        BOPAlgo_Builder * builder, TopoDS_Shape * shape,
        HSExceptionType* exType,
        void** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [builder, shape]{
        builder->AddArgument(*shape);
    });
}

TopoDS_Shape * hs_BOPAlgo_Builder_Shape(
        BOPAlgo_Builder * builder,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [builder]{
        return new TopoDS_Shape(builder->Shape());
    });
}

void hs_BOPAlgo_Builder_SetRunParallel(BOPAlgo_Builder * builder, bool runParallel){
    builder->SetRunParallel(runParallel);
}

void hs_BOPAlgo_Builder_Perform(
        BOPAlgo_Builder * builder,
        HSExceptionType* exType,
        void** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [builder]{
        builder->Perform();
    });
}


LIST(TopoDS_Shape) * hs_BOPAlgo_Builder_Modified(
        BOPAlgo_Builder * builder, TopoDS_Shape * shape,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [builder, shape]{
        return new TopTools_ListOfShape(builder->Modified(*shape));
    });
}

LIST(TopoDS_Shape) * hs_BOPAlgo_Builder_Generated(
        BOPAlgo_Builder * builder, TopoDS_Shape * shape,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [builder, shape]{
        return new TopTools_ListOfShape(builder->Generated(*shape));
    });
}

bool hs_BOPAlgo_Builder_IsDeleted(
        BOPAlgo_Builder * builder, TopoDS_Shape * shape,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleExWithDefault(
        exType,
        exPtr,
        [builder, shape]{
        return builder->IsDeleted(*shape) == Standard_True;
    },
    false);
}
