#include <BRepGProp.hxx>
#include "hs_BRepGProp.h"

void hs_BRepGProp_VolumeProperties(TopoDS_Shape *shape, GProp_GProps *props, bool onlyClosed, bool skipShared, bool useTriangulation ){
    BRepGProp::VolumeProperties(*shape, *props, onlyClosed, skipShared, useTriangulation);
}

void hs_BRepGProp_SurfaceProperties(TopoDS_Shape *shape, GProp_GProps *props, bool skipShared, bool useTriangulation ){
    BRepGProp::LinearProperties(*shape, *props, skipShared, useTriangulation);
}

void hs_BRepGProp_LinearProperties(TopoDS_Shape *shape, GProp_GProps *props, bool skipShared, bool useTriangulation ){
    BRepGProp::LinearProperties(*shape, *props, skipShared, useTriangulation);
}