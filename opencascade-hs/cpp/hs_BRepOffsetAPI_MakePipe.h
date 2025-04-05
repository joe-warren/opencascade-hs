#ifndef HS_BREPOFFSETAPI_MAKEPIPE_H
#define HS_BREPOFFSETAPI_MAKEPIPE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepOffsetAPI_MakePipe * hs_new_BRepOffsetAPI_MakePipe_fromWireAndShape(TopoDS_Wire * wire, TopoDS_Shape * profile);

BRepOffsetAPI_MakePipe * hs_new_BRepOffsetAPI_MakePipe_fromWireShapeTrihedronModeAndForceC1(
    TopoDS_Wire * wire,
    TopoDS_Shape * profile,
    GeomFill_Trihedron mode, 
    bool forceApproxC1 );

void hs_delete_BRepOffsetAPI_MakePipe(BRepOffsetAPI_MakePipe * builder);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPOFFSETAPI_MAKEPIPE_H
