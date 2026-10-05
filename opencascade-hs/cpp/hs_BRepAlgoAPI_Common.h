
#ifndef HS_BREPALGOAPI_COMMON_H
#define HS_BREPALGOAPI_COMMON_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepAlgoAPI_Common * hs_new_BRepAlgoAPI_Common_fromShapes(
    TopoDS_Shape * a, TopoDS_Shape * b,
    HSExceptionType* exType,
    void** exPtr
);

void hs_delete_BRepAlgoAPI_Common(BRepAlgoAPI_Common * builder);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPALGOAPI_COMMON_H
