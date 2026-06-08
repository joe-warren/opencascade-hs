
#ifndef HS_BREPALGOAPI_COMMON_H
#define HS_BREPALGOAPI_COMMON_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

TopoDS_Shape * hs_BRepAlgoAPI_Common(
    TopoDS_Shape * a, TopoDS_Shape * b,
    HSExceptionType* exType,
    void** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPALGOAPI_COMMON_H
