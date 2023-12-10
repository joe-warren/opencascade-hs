#ifndef HS_BREPOFFSETAPI_MAKEOFFSETSHAPE_H
#define HS_BREPOFFSETAPI_MAKEOFFSETSHAPE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepOffsetAPI_MakeOffsetShape * hs_new_BRepOffsetAPI_MakeOffsetShape();

void hs_delete_BRepOffsetAPI_MakeOffsetShape(BRepOffsetAPI_MakeOffsetShape * builder);

void hs_BRepOffsetAPI_MakeOffsetShape_performBySimple(BRepOffsetAPI_MakeOffsetShape * builder, TopoDS_Shape * shape, double value);

void hs_BRepOffsetAPI_MakeOffsetShape_performByJoin(
    BRepOffsetAPI_MakeOffsetShape * builder,
    TopoDS_Shape * shape, 
    double value,
    double tol,
    BRepOffset_Mode mode,
    bool intersection,
    bool selfInter,
    GeomAbs_JoinType join,
    bool removeIntEdges
    );

#ifdef __cplusplus
}
#endif

#endif // HS_BREPOFFSETAPI_MAKEOFFSETSHAPE_H
