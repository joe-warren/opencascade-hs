#ifndef HS_BOPALGO_CELLSBUILDER_H
#define HS_BOPALGO_CELLSBUILDER_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BOPAlgo_CellsBuilder * hs_new_BOPAlgo_CellsBuilder();

void hs_delete_BOPAlgo_CellsBuilder(BOPAlgo_CellsBuilder * builder);

void hs_BOPAlgo_CellsBuilder_AddToResult(
    BOPAlgo_CellsBuilder * builder,
    LIST(TopoDS_Shape) * toTake,
    LIST(TopoDS_Shape) * toAvoid,
    int material,
    bool update,
    HSExceptionType* exType,
    void** exPtr
);

void hs_BOPAlgo_CellsBuilder_AddAllToResult(
    BOPAlgo_CellsBuilder * builder,
    int material,
    bool update,
    HSExceptionType* exType,
    void** exPtr
);

void hs_BOPAlgo_CellsBuilder_RemoveFromResult(
    BOPAlgo_CellsBuilder * builder,
    LIST(TopoDS_Shape) * toTake,
    LIST(TopoDS_Shape) * toAvoid,
    HSExceptionType* exType,
    void** exPtr
);

void hs_BOPAlgo_CellsBuilder_RemoveAllFromResult(
    BOPAlgo_CellsBuilder * builder,
    HSExceptionType* exType,
    void** exPtr
);

void hs_BOPAlgo_CellsBuilder_RemoveInternalBoundaries(
    BOPAlgo_CellsBuilder * builder,
    HSExceptionType* exType,
    void** exPtr
);

TopoDS_Shape * hs_BOPAlgo_CellsBuilder_GetAllParts(
    BOPAlgo_CellsBuilder * builder,
    HSExceptionType* exType,
    void** exPtr
);

void hs_BOPAlgo_CellsBuilder_MakeContainers(
    BOPAlgo_CellsBuilder * builder,
    HSExceptionType* exType,
    void** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_BOPALGO_CELLSBUILDER_H
