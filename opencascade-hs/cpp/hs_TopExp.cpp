#include <TopExp.hxx>
#include <TopoDS_Shape.hxx>
#include "hs_Exception.h"
#include "hs_TopExp.h"

void hs_TopExp_mapShapesAndAncestors(
        TopoDS_Shape * shape, TopAbs_ShapeEnum subshapeType, TopAbs_ShapeEnum ancestorType,
        INDEXED_DATA_MAP(TopoDS_Shape, LIST(TopoDS_Shape), TopTools_ShapeMapHasher) * theMap,
        HSExceptionType* exType, void ** exPtr
    ){
    hs_handleExVoid(exType, exPtr, [shape, subshapeType, ancestorType, theMap]{
        TopExp::MapShapesAndAncestors(*shape, subshapeType, ancestorType, *theMap);
    });
}
