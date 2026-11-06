#include <BRepGProp.hxx>
#include "hs_Exception.h"
#include "hs_BRepGProp.h"


void hs_BRepGProp_VolumeProperties(
        TopoDS_Shape *shape, GProp_GProps *props, bool onlyClosed, bool skipShared, bool useTriangulation,
        HSExceptionType* exType, void ** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [shape, props, onlyClosed, skipShared, useTriangulation]{
            BRepGProp::VolumeProperties(*shape, *props, onlyClosed, skipShared, useTriangulation);
        }
    );
}

void hs_BRepGProp_VolumePropertiesWithEps(
        TopoDS_Shape *shape, GProp_GProps *props, double eps, bool onlyClosed, bool skipShared,
        HSExceptionType* exType, void ** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [shape, props, eps, onlyClosed, skipShared]{
            BRepGProp::VolumeProperties(*shape, *props, eps, onlyClosed, skipShared);
        }
    );
}

void hs_BRepGProp_SurfaceProperties(
        TopoDS_Shape *shape, GProp_GProps *props, bool skipShared, bool useTriangulation,
        HSExceptionType* exType, void ** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [shape, props, skipShared, useTriangulation]{
            BRepGProp::SurfaceProperties(*shape, *props, skipShared, useTriangulation);
        }
    );
}

void hs_BRepGProp_SurfacePropertiesWithEps(
        TopoDS_Shape *shape, GProp_GProps *props, double eps, bool skipShared,
        HSExceptionType* exType, void ** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [shape, props, eps, skipShared]{
            BRepGProp::SurfaceProperties(*shape, *props, eps, skipShared);
        }
    );
}

void hs_BRepGProp_LinearProperties(
        TopoDS_Shape *shape, GProp_GProps *props, bool skipShared, bool useTriangulation,
        HSExceptionType* exType, void ** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [shape, props, skipShared, useTriangulation]{
            BRepGProp::LinearProperties(*shape, *props, skipShared, useTriangulation);
        }
    );
}