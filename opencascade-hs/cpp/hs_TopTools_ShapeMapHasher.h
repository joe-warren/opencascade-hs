#ifndef HS_TOPTOOLS_SHAPEMAPHASHER_H
#define HS_TOPTOOLS_SHAPEMAPHASHER_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

int hs_TopTools_ShapeMapHasher_hash(TopoDS_Shape * shape, HSExceptionType* exType, void ** exPtr);

bool hs_TopTools_ShapeMapHasher_isEqual(TopoDS_Shape * shapeA, TopoDS_Shape * shapeB, HSExceptionType* exType, void ** exPtr);

#ifdef __cplusplus
}
#endif

#endif // HS_TOPTOOLS_SHAPEMAPHASHER_H
