#ifndef HS_BREPALGOAPI_BOOLEANOPERATION_H
#define HS_BREPALGOAPI_BOOLEANOPERATION_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

void hs_BRepAlgoAPI_BooleanOperation_setTools(
    BRepAlgoAPI_BooleanOperation * builder, 
    LIST(TopoDS_Shape) * tools, 
    HSExceptionType* exType,
    void** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPALGOAPI_BOOLEANOPERATION_H
