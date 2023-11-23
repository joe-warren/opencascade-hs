#ifndef HS_BREPFILLET_API_MAKEFILLET_H
#define HS_BREPFILLET_API_MAKEFILLET_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepFilletAPI_MakeFillet * hs_new_BRepFilletAPI_MakeFillet_fromShape(TopoDS_Shape * shape);

void hs_delete_BRepFilletAPI_MakeFillet(BRepFilletAPI_MakeFillet * builder);

void hs_BRepFilletAPI_MakeFillet_addEdge(BRepFilletAPI_MakeFillet * builder, TopoDS_Edge *edge );

void hs_BRepFilletAPI_MakeFillet_addEdgeWithRadius(BRepFilletAPI_MakeFillet * builder, double r, TopoDS_Edge *edge );

void hs_BRepFilletAPI_MakeFillet_addEdgeWithTwoRadiuses(BRepFilletAPI_MakeFillet * builder, double r1, double r2, TopoDS_Edge *edge );

#ifdef __cplusplus
}
#endif

#endif // HS_BREPFILLET_API_MAKEFILLET_H
