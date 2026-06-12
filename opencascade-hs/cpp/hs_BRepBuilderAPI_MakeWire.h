#ifndef HS_BREPBUILDERAPI_MAKEWIRE_H
#define HS_BREPBUILDERAPI_MAKEWIRE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepBuilderAPI_MakeWire * hs_new_BRepBuilderAPI_MakeWire();

void hs_delete_BRepBuilderAPI_MakeWire(BRepBuilderAPI_MakeWire* builder);

void hs_BRepBuilderAPI_MakeWire_AddEdge(
    BRepBuilderAPI_MakeWire* builder, TopoDS_Edge* edge,
    HSExceptionType* exType,
    void** exPtr
);

void hs_BRepBuilderAPI_MakeWire_AddWire(
    BRepBuilderAPI_MakeWire* builder, TopoDS_Wire* wire,
    HSExceptionType* exType,
    void** exPtr
);

void hs_BRepBuilderAPI_MakeWire_AddListOfShape(
    BRepBuilderAPI_MakeWire* builder, TopTools_ListOfShape *list,
    HSExceptionType* exType,
    void** exPtr
);

TopoDS_Wire * hs_BRepBuilderAPI_MakeWire_Wire(
    BRepBuilderAPI_MakeWire* builder,
    HSExceptionType* exType,
    void** exPtr
);

TopoDS_Vertex * hs_BRepBuilderAPI_MakeWire_Vertex(
    BRepBuilderAPI_MakeWire* builder,
    HSExceptionType* exType,
    void** exPtr
);

bool hs_BRepBuilderAPI_MakeWire_IsDone(BRepBuilderAPI_MakeWire* builder);

BRepBuilderAPI_WireError hs_BRepBuilderAPI_MakeWire_Error(BRepBuilderAPI_MakeWire* builder);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPBUILDERAPI_MAKEWIRE_H
