#include <BRepFilletAPI_MakeFillet.hxx>
#include <TopoDS_Shape.hxx>
#include "hs_BRepFilletAPI_MakeFillet.h"

BRepFilletAPI_MakeFillet * hs_new_BRepFilletAPI_MakeFillet_fromShape(TopoDS_Shape * shape) {
    return new BRepFilletAPI_MakeFillet(*shape);
}

void hs_delete_BRepFilletAPI_MakeFillet(BRepFilletAPI_MakeFillet * builder) {
    delete builder;
}

void hs_BRepFilletAPI_MakeFillet_addEdge(BRepFilletAPI_MakeFillet * builder, TopoDS_Edge *edge ){
    builder->Add(*edge);
}

void hs_BRepFilletAPI_MakeFillet_addEdgeWithRadius(BRepFilletAPI_MakeFillet * builder, double r, TopoDS_Edge *edge ){
    builder->Add(r, *edge);
}

void hs_BRepFilletAPI_MakeFillet_addEdgeWithTwoRadiuses(BRepFilletAPI_MakeFillet * builder, double r1, double r2, TopoDS_Edge *edge ){
    builder->Add(r1, r2, *edge);
}

void hs_BRepFilletAPI_MakeFillet_reset(BRepFilletAPI_MakeFillet * builder){
    builder->Reset();
}

int hs_BRepFilletAPI_MakeFillet_nbFaultyContours(BRepFilletAPI_MakeFillet * builder){
    return builder->NbFaultyContours();
}

int hs_BRepFilletAPI_MakeFillet_faultyContour(BRepFilletAPI_MakeFillet * builder, int index){
    return builder->FaultyContour(index);
}

int hs_BRepFilletAPI_MakeFillet_nbEdges(BRepFilletAPI_MakeFillet * builder, int contourIndex){
    return builder->NbEdges(contourIndex);
}

TopoDS_Edge * hs_BRepFilletAPI_MakeFillet_edge(BRepFilletAPI_MakeFillet * builder, int contourIndex, int edgeIndex){
    return new TopoDS_Edge(builder->Edge(contourIndex, edgeIndex));
}

void hs_BRepFilletAPI_MakeFillet_remove(BRepFilletAPI_MakeFillet * builder, TopoDS_Edge * edge){
    builder->Remove(*edge);
}