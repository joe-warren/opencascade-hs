#include <TopExp.hxx>
#include <TopoDS_Shape.hxx>
#include <TopTools_IndexedDataMapOfShapeListOfShape.hxx>
#include "hs_Exception.h"
#include "hs_TopExp.h"

TopTools_IndexedDataMapOfShapeListOfShape * hs_TopExp_mapShapesAndAncestors(
        TopoDS_Shape * shape, TopAbs_ShapeEnum subshapeType, TopAbs_ShapeEnum ancestorType,
        HSExceptionType* exType, void ** exPtr
    ){
    return hs_handleEx(exType, exPtr, [shape, subshapeType, ancestorType]{
        auto map = new TopTools_IndexedDataMapOfShapeListOfShape();
        TopExp::MapShapesAndAncestors(*shape, subshapeType, ancestorType, *map);
        return map;
    });
}
