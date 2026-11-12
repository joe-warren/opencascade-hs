#include <BOPAlgo_CellsBuilder.hxx>
#include <NCollection_List.hxx>
#include <TopoDS_Shape.hxx>
#include "hs_Exception.h"
#include "hs_BOPAlgo_CellsBuilder.h"

BOPAlgo_CellsBuilder * hs_new_BOPAlgo_CellsBuilder(){
    return new BOPAlgo_CellsBuilder();
}

void hs_delete_BOPAlgo_CellsBuilder(BOPAlgo_CellsBuilder * builder){
    delete builder;
}

void hs_BOPAlgo_CellsBuilder_AddToResult(
        BOPAlgo_CellsBuilder * builder,
        LIST(TopoDS_Shape) * toTake,
        LIST(TopoDS_Shape) * toAvoid,
        int material,
        bool update,
        HSExceptionType* exType,
        void** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [builder, toTake, toAvoid, material, update]{
        builder->AddToResult(*toTake, *toAvoid, material, update);
    });
}

void hs_BOPAlgo_CellsBuilder_AddAllToResult(
        BOPAlgo_CellsBuilder * builder,
        int material,
        bool update,
        HSExceptionType* exType,
        void** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [builder, material, update]{
        builder->AddAllToResult(material, update);
    });
}

void hs_BOPAlgo_CellsBuilder_RemoveFromResult(
        BOPAlgo_CellsBuilder * builder,
        LIST(TopoDS_Shape) * toTake,
        LIST(TopoDS_Shape) * toAvoid,
        HSExceptionType* exType,
        void** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [builder, toTake, toAvoid]{
        builder->RemoveFromResult(*toTake, *toAvoid);
    });
}

void hs_BOPAlgo_CellsBuilder_RemoveAllFromResult(
        BOPAlgo_CellsBuilder * builder,
        HSExceptionType* exType,
        void** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [builder]{
        builder->RemoveAllFromResult();
    });
}

void hs_BOPAlgo_CellsBuilder_RemoveInternalBoundaries(
        BOPAlgo_CellsBuilder * builder,
        HSExceptionType* exType,
        void** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [builder]{
        builder->RemoveInternalBoundaries();
    });
}

TopoDS_Shape * hs_BOPAlgo_CellsBuilder_GetAllParts(
        BOPAlgo_CellsBuilder * builder,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [builder]{
        return new TopoDS_Shape(builder->GetAllParts());
    });
}

void hs_BOPAlgo_CellsBuilder_MakeContainers(
        BOPAlgo_CellsBuilder * builder,
        HSExceptionType* exType,
        void** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [builder]{
        builder->MakeContainers();
    });
}
