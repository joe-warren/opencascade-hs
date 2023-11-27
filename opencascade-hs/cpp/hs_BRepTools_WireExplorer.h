#ifndef HS_BREPTOOLS_WIREEXPLORER_H
#define HS_BREPTOOLS_WIREEXPLORER_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif


BRepTools_WireExplorer * hs_new_BRepTools_WireExplorer_fromWire(TopoDS_Wire *wire);

void hs_delete_BRepTools_WireExplorer(BRepTools_WireExplorer * explorer);

bool hs_BRepTools_WireExplorer_more(BRepTools_WireExplorer * explorer);

void hs_BRepTools_WireExplorer_next(BRepTools_WireExplorer * explorer);

TopoDS_Edge * hs_BRepTools_WireExplorer_current(BRepTools_WireExplorer * explorer);

TopAbs_Orientation hs_BRepTools_WireExplorer_orientation(BRepTools_WireExplorer * explorer);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPTOOLS_WIREEXPLORER_H
