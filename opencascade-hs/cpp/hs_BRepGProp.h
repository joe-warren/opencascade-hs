#ifndef HS_BREPGPROP_H
#define HS_BREPGPROP_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

void hs_BRepGProp_VolumeProperties(
    TopoDS_Shape * shape, GProp_GProps * props, bool onlyClosed, bool skipShared, bool useTriangulation,
    HSExceptionType* exType, void ** exPtr
);

void hs_BRepGProp_SurfaceProperties(
    TopoDS_Shape *shape, GProp_GProps *props, bool skipShared, bool useTriangulation,
    HSExceptionType* exType, void ** exPtr
);

void hs_BRepGProp_LinearProperties(
    TopoDS_Shape *shape, GProp_GProps *props, bool skipShared, bool useTriangulation,
    HSExceptionType* exType, void ** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPGPROP_H
