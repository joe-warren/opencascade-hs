#include <BRepGProp.hxx>
#include "hs_BRepGProp.h"

void hs_BRepGProp_VolumeProperties(TopoDS_Shape *shape, GProp_GProps *props, bool onlyClosed, bool skipShared, bool useTriangulation ){
    BRepGProp::VolumeProperties(*shape, *props, onlyClosed, skipShared, useTriangulation);
}