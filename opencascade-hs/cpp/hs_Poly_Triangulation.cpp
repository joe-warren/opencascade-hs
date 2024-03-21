#include <Poly_Triangulation.hxx>
#include "hs_Poly_Triangulation.h"

Handle(Poly_Triangulation) * hs_new_Poly_Triangulation(int nbNodes, int nbTriangles, bool hasUVNodes, bool hasNormals){
    return new opencascade::handle<Poly_Triangulation>(new Poly_Triangulation(nbNodes, nbTriangles, hasUVNodes, hasNormals));
}

void hs_delete_Poly_Triangulation(Handle(Poly_Triangulation) * triangulation){
    delete triangulation;
}

int hs_Poly_Triangulation_nbNodes(Handle(Poly_Triangulation) * triangulation){
    return triangulation->get()->NbNodes();
}

int hs_Poly_Triangulation_nbTriangles(Handle(Poly_Triangulation) * triangulation){
    return triangulation->get()->NbTriangles();
}


gp_Pnt * hs_Poly_Triangulation_node(Handle(Poly_Triangulation) * triangulation, int index){
    return new gp_Pnt(triangulation->get()->Node(index));
}

void hs_Poly_Triangulation_setNode(Handle(Poly_Triangulation) * triangulation, int index, gp_Pnt * pnt){
    triangulation->get()->SetNode(index, *pnt);
}

Poly_Triangle * hs_Poly_Triangulation_triangle(Handle(Poly_Triangulation) * triangulation, int index){
    return new Poly_Triangle(triangulation->get()->Triangle(index));
}

void hs_Poly_Triangulation_setTriangle(Handle(Poly_Triangulation) * triangulation, int index, Poly_Triangle * triangle){
    triangulation->get()->SetTriangle(index, *triangle);
}