#include <TopExp.hxx>
#include <TopoDS_Shape.hxx>
#include <TopoDS_Edge.hxx>
#include <TopoDS_Vertex.hxx>
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

TopoDS_Vertex * hs_TopExp_firstVertex(
        TopoDS_Edge * edge, bool cumOri,
        HSExceptionType* exType, void ** exPtr
    ){
    return hs_handleEx(exType, exPtr, [edge, cumOri]{
        return new TopoDS_Vertex(TopExp::FirstVertex(*edge, cumOri));
    });
}

TopoDS_Vertex * hs_TopExp_lastVertex(
        TopoDS_Edge * edge, bool cumOri,
        HSExceptionType* exType, void ** exPtr
    ){
    return hs_handleEx(exType, exPtr, [edge, cumOri]{
        return new TopoDS_Vertex(TopExp::LastVertex(*edge, cumOri));
    });
}
