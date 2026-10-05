
#ifndef HS_BREPALGOAPI_CUT_H
#define HS_BREPALGOAPI_CUT_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepAlgoAPI_Cut * hs_new_BRepAlgoAPI_Cut_fromShapes(
    TopoDS_Shape * a, TopoDS_Shape * b,
    HSExceptionType* exType,
    void** exPtr
);

void hs_delete_BRepAlgoAPI_Cut(BRepAlgoAPI_Cut * builder);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPALGOAPI_CUT_H
