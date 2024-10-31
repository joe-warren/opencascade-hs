#ifndef HS_BREPFILLETAPI_MAKEFILLET_H
#define HS_BREPFILLETAPI_MAKEFILLET_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepFilletAPI_MakeFillet * hs_new_BRepFilletAPI_MakeFillet_fromShape(TopoDS_Shape * shape);

void hs_delete_BRepFilletAPI_MakeFillet(BRepFilletAPI_MakeFillet * builder);

void hs_BRepFilletAPI_MakeFillet_addEdge(BRepFilletAPI_MakeFillet * builder, TopoDS_Edge *edge );

void hs_BRepFilletAPI_MakeFillet_addEdgeWithRadius(BRepFilletAPI_MakeFillet * builder, double r, TopoDS_Edge *edge );

void hs_BRepFilletAPI_MakeFillet_addEdgeWithTwoRadiuses(BRepFilletAPI_MakeFillet * builder, double r1, double r2, TopoDS_Edge *edge );

void hs_BRepFilletAPI_MakeFillet_reset(BRepFilletAPI_MakeFillet * builder);

int hs_BRepFilletAPI_MakeFillet_nbFaultyContours(BRepFilletAPI_MakeFillet * builder);

int hs_BRepFilletAPI_MakeFillet_faultyContour(BRepFilletAPI_MakeFillet * builder, int index);

int hs_BRepFilletAPI_MakeFillet_nbEdges(BRepFilletAPI_MakeFillet * builder, int contourIndex);

TopoDS_Edge * hs_BRepFilletAPI_MakeFillet_edge(BRepFilletAPI_MakeFillet * builder, int contourIndex, int edgeIndex);

void hs_BRepFilletAPI_MakeFillet_remove(BRepFilletAPI_MakeFillet * builder, TopoDS_Edge * edge);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPFILLETAPI_MAKEFILLET_H
