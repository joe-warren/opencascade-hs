
#ifndef HS_BREPALGOAPI_CUT_H
#define HS_BREPALGOAPI_CUT_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

TopoDS_Shape * hs_BRepAlgoAPI_Cut(TopoDS_Shape * a, TopoDS_Shape * b);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPALGOAPI_CUT_H
