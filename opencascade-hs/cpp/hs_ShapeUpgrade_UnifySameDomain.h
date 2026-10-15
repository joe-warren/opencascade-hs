#ifndef HS_SHAPEUPGRADE_UNIFYSAMEDOMAIN_H
#define HS_SHAPEUPGRADE_UNIFYSAMEDOMAIN_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

ShapeUpgrade_UnifySameDomain * hs_new_ShapeUpgrade_UnifySameDomain_fromShape(
    TopoDS_Shape * shape, bool unifyEdges, bool unifyFaces, bool concatBSplines,
    HSExceptionType* exType, void ** exPtr
);

void hs_delete_ShapeUpgrade_UnifySameDomain(ShapeUpgrade_UnifySameDomain * unify);

void hs_ShapeUpgrade_UnifySameDomain_setSafeInputMode(ShapeUpgrade_UnifySameDomain * unify, bool value);

void hs_ShapeUpgrade_UnifySameDomain_allowInternalEdges(ShapeUpgrade_UnifySameDomain * unify, bool value);

void hs_ShapeUpgrade_UnifySameDomain_build(
    ShapeUpgrade_UnifySameDomain * unify,
    HSExceptionType* exType, void ** exPtr
);

TopoDS_Shape * hs_ShapeUpgrade_UnifySameDomain_shape(ShapeUpgrade_UnifySameDomain * unify);

#ifdef __cplusplus
}
#endif

#endif // HS_SHAPEUPGRADE_UNIFYSAMEDOMAIN_H
