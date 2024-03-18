#include <BRepBuilderAPI_Copy.hxx>
#include "hs_BRepBuilderAPI_Copy.h"

TopoDS_Shape * hs_BRepBuilderAPI_Copy_copy(TopoDS_Shape *shape, bool copyGeom, bool copyMesh){
    return new TopoDS_Shape(BRepBuilderAPI_Copy(*shape, copyGeom, copyMesh).Shape());
}