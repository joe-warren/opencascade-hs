#ifndef HS_BREPALGOAPI_SECTION_H
#define HS_BREPALGOAPI_SECTION_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepAlgoAPI_Section * hs_new_BRepAlgoAPI_Section_fromShapes(
    TopoDS_Shape * a, TopoDS_Shape * b,
    HSExceptionType* exType,
    void** exPtr
);

void hs_delete_BRepAlgoAPI_Section(BRepAlgoAPI_Section * builder);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPALGOAPI_SECTION_H
