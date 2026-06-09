#include <Poly_Triangulation.hxx>
#include "hs_Exception.h"
#include "hs_Poly_Triangulation.h"

Handle(Poly_Triangulation) * hs_new_Poly_Triangulation(
        int nbNodes, int nbTriangles, bool hasUVNodes, bool hasNormals,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [nbNodes, nbTriangles, hasUVNodes, hasNormals]{
            return new opencascade::handle<Poly_Triangulation>(new Poly_Triangulation(nbNodes, nbTriangles, hasUVNodes, hasNormals));
        }
    );
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


gp_Pnt * hs_Poly_Triangulation_node(
        Handle(Poly_Triangulation) * triangulation, int index,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [triangulation, index]{
            return new gp_Pnt(triangulation->get()->Node(index));
        }
    );
}

void hs_Poly_Triangulation_setNode(
        Handle(Poly_Triangulation) * triangulation, int index, gp_Pnt * pnt,
        HSExceptionType* exType, void ** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [triangulation, index, pnt]{
            triangulation->get()->SetNode(index, *pnt);
        }
    );
}

Poly_Triangle * hs_Poly_Triangulation_triangle(
        Handle(Poly_Triangulation) * triangulation, int index,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [triangulation, index]{
            return new Poly_Triangle(triangulation->get()->Triangle(index));
        }
    );
}

void hs_Poly_Triangulation_setTriangle(
        Handle(Poly_Triangulation) * triangulation, int index, Poly_Triangle * triangle,
        HSExceptionType* exType, void ** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [triangulation, index, triangle]{
            triangulation->get()->SetTriangle(index, *triangle);
        }
    );
}