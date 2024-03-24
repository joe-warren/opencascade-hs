#ifndef HS_POLY_TRIANGULATION_H
#define HS_POLY_TRIANGULATION_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

Handle(Poly_Triangulation) * hs_new_Poly_Triangulation(int nbNodes, int nbTriangles, bool hasUVNodes, bool hasNormals);

void hs_delete_Poly_Triangulation(Handle(Poly_Triangulation) * triangulation);

int hs_Poly_Triangulation_nbNodes(Handle(Poly_Triangulation) * triangulation);

int hs_Poly_Triangulation_nbTriangles(Handle(Poly_Triangulation) * triangulation);

gp_Pnt * hs_Poly_Triangulation_node(Handle(Poly_Triangulation) * triangulation, int index);

void hs_Poly_Triangulation_setNode(Handle(Poly_Triangulation) * triangulation, int index, gp_Pnt * pnt);

Poly_Triangle * hs_Poly_Triangulation_triangle(Handle(Poly_Triangulation) * triangulation, int index);

void hs_Poly_Triangulation_setTriangle(Handle(Poly_Triangulation) * triangulation, int index, Poly_Triangle * triangle);

#ifdef __cplusplus
}
#endif

#endif // HS_POLY_TRIANGULATION_H
