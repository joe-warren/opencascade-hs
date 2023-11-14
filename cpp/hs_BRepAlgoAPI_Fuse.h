
#ifndef HS_BREPALGOAPI_FUSE_H
#define HS_BREPALGOAPI_FUSE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

TopoDS_Shape * hs_BRepAlgoAPI_Fuse(TopoDS_Shape * a, TopoDS_Shape * b);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPALGOAPI_FUSE_H
