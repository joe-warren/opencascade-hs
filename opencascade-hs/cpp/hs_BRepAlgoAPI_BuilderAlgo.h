#ifndef HS_BREPALGOAPI_BUILDERALGO_H
#define HS_BREPALGOAPI_BUILDERALGO_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

void hs_BRepAlgoAPI_BuilderAlgo_setNonDestructive(BRepAlgoAPI_BuilderAlgo * builder, bool nonDestructive);

void hs_BRepAlgoAPI_BuilderAlgo_setArguments(
    BRepAlgoAPI_BuilderAlgo * builder, 
    LIST(TopoDS_Shape) * arguments, 
    HSExceptionType* exType,
    void** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPALGOAPI_BUILDERALGO_H
