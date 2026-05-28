#include <BRepOffsetAPI_MakeOffsetShape.hxx>
#include "hs_BRepOffsetAPI_MakeOffsetShape.h"
#include "hs_Exception.h"

BRepOffsetAPI_MakeOffsetShape * hs_new_BRepOffsetAPI_MakeOffsetShape(){
    return new BRepOffsetAPI_MakeOffsetShape();
}

void hs_delete_BRepOffsetAPI_MakeOffsetShape(BRepOffsetAPI_MakeOffsetShape * builder){
    delete builder;
}

void hs_BRepOffsetAPI_MakeOffsetShape_performBySimple(BRepOffsetAPI_MakeOffsetShape * builder, TopoDS_Shape * shape, double value){
    builder->PerformBySimple(*shape, value);
}

void hs_BRepOffsetAPI_MakeOffsetShape_performByJoin(
    BRepOffsetAPI_MakeOffsetShape * builder,
    TopoDS_Shape * shape, 
    double value,
    double tol,
    BRepOffset_Mode mode,
    bool intersection,
    bool selfInter,
    GeomAbs_JoinType join,
    bool removeIntEdges,
    HSExceptionType* exType,
    void* exPtr
     ){
    hs_handleEx(
        exType,
        exPtr,
        builder,
        &BRepOffsetAPI_MakeOffsetShape::PerformByJoin,
        *shape,
        value,
        tol,
        mode,
        intersection,
        selfInter,
        join,
        removeIntEdges,
        Message_ProgressRange()
    );
}