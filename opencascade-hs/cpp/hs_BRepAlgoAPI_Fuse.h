#ifndef HS_BREPALGOAPI_FUSE_H
#define HS_BREPALGOAPI_FUSE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepAlgoAPI_Fuse * hs_new_BRepAlgoAPI_Fuse_fromShapes(
    TopoDS_Shape * a, TopoDS_Shape * b,
    HSExceptionType* exType,
    void** exPtr
);

void hs_delete_BRepAlgoAPI_Fuse(BRepAlgoAPI_Fuse * builder);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPALGOAPI_FUSE_H
