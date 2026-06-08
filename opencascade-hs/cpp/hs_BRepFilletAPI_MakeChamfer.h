#ifndef HS_BREPFILLETAPI_MAKECHAMFER_H
#define HS_BREPFILLETAPI_MAKECHAMFER_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepFilletAPI_MakeChamfer * hs_new_BRepFilletAPI_MakeChamfer_fromShape(
    TopoDS_Shape * shape,
    HSExceptionType* exType,
    void** exPtr
);

void hs_delete_BRepFilletAPI_MakeChamfer(BRepFilletAPI_MakeChamfer * builder) ;

void hs_BRepFilletAPI_MakeChamfer_addEdge(
    BRepFilletAPI_MakeChamfer * builder, TopoDS_Edge *edge,
    HSExceptionType* exType,
    void** exPtr
);

void hs_BRepFilletAPI_MakeChamfer_addEdgeWithDistance(
    BRepFilletAPI_MakeChamfer * builder, double d, TopoDS_Edge *edge,
    HSExceptionType* exType,
    void** exPtr
);

void hs_BRepFilletAPI_MakeChamfer_reset(BRepFilletAPI_MakeChamfer * builder);

int hs_BRepFilletAPI_MakeChamfer_nbEdges(BRepFilletAPI_MakeChamfer * builder, int contourIndex);

TopoDS_Edge * hs_BRepFilletAPI_MakeChamfer_edge(
    BRepFilletAPI_MakeChamfer * builder, int contourIndex, int edgeIndex,
    HSExceptionType* exType,
    void** exPtr
);

void hs_BRepFilletAPI_MakeChamfer_remove(
    BRepFilletAPI_MakeChamfer * builder, TopoDS_Edge * edge,
    HSExceptionType* exType,
    void** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPFILLETAPI_MAKECHAMFER_H
