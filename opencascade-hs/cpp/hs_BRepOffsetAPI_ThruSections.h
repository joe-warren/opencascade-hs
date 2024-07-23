
#ifndef HS_BREPOFFSETAPI_THRUSECTIONS_H
#define HS_BREPOFFSETAPI_THRUSECTIONS_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepOffsetAPI_ThruSections * hs_new_BRepOffsetAPI_ThruSections(bool isSolid, bool ruled, double pres3d);

void hs_delete_BRepOffsetAPI_ThruSections(BRepOffsetAPI_ThruSections* thruSections);

void hs_BRepOffsetAPI_ThruSections_addWire(BRepOffsetAPI_ThruSections* thruSections, TopoDS_Wire * wire);

void hs_BRepOffsetAPI_ThruSections_addVertex(BRepOffsetAPI_ThruSections* thruSections, TopoDS_Vertex* vertex);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPOFFSETAPI_THRUSECTIONS_H
