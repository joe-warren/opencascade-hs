#ifndef HS_BOPALGO_BUILDER_H
#define HS_BOPALGO_BUILDER_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BOPAlgo_Builder * hs_new_BOPAlgo_Builder();

void hs_delete_BOPAlgo_Builder(BOPAlgo_Builder * builder);

void hs_BOPAlgo_Builder_AddArgument(
    BOPAlgo_Builder * builder, TopoDS_Shape * shape,
    HSExceptionType* exType,
    void** exPtr
);

TopoDS_Shape * hs_BOPAlgo_Builder_Shape(
    BOPAlgo_Builder * builder,
    HSExceptionType* exType,
    void** exPtr
);

void hs_BOPAlgo_Builder_SetRunParallel(BOPAlgo_Builder * builder, bool runParallel);

void hs_BOPAlgo_Builder_Perform(
    BOPAlgo_Builder * builder,
    HSExceptionType* exType,
    void** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_BOPALGO_BUILDER_H
