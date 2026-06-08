#include <BRepFilletAPI_MakeChamfer.hxx>
#include <TopoDS_Shape.hxx>
#include "hs_Exception.h"
#include "hs_BRepFilletAPI_MakeChamfer.h"

BRepFilletAPI_MakeChamfer * hs_new_BRepFilletAPI_MakeChamfer_fromShape(
        TopoDS_Shape * shape,
        HSExceptionType* exType,
        void** exPtr
) {
    return hs_handleEx(
        exType,
        exPtr,
        [shape]{
        return new BRepFilletAPI_MakeChamfer(*shape);
    });
}

void hs_delete_BRepFilletAPI_MakeChamfer(BRepFilletAPI_MakeChamfer * builder) {
    delete builder;
}

void hs_BRepFilletAPI_MakeChamfer_addEdge(
        BRepFilletAPI_MakeChamfer * builder, TopoDS_Edge *edge,
        HSExceptionType* exType,
        void** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [builder, edge]{
        builder->Add(*edge);
    });
}

void hs_BRepFilletAPI_MakeChamfer_addEdgeWithDistance(
        BRepFilletAPI_MakeChamfer * builder, double d, TopoDS_Edge *edge,
        HSExceptionType* exType,
        void** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [builder, d, edge]{
        builder->Add(d, *edge);
    });
}

void hs_BRepFilletAPI_MakeChamfer_reset(BRepFilletAPI_MakeChamfer * builder){
    builder->Reset();
}

int hs_BRepFilletAPI_MakeChamfer_nbEdges(BRepFilletAPI_MakeChamfer * builder, int contourIndex){
    return builder->NbEdges(contourIndex);
}

TopoDS_Edge * hs_BRepFilletAPI_MakeChamfer_edge(
        BRepFilletAPI_MakeChamfer * builder, int contourIndex, int edgeIndex,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [builder, contourIndex, edgeIndex]{
        return new TopoDS_Edge(builder->Edge(contourIndex, edgeIndex));
    });
}

void hs_BRepFilletAPI_MakeChamfer_remove(
        BRepFilletAPI_MakeChamfer * builder, TopoDS_Edge * edge,
        HSExceptionType* exType,
        void** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [builder, edge]{
        builder->Remove(*edge);
    });
}