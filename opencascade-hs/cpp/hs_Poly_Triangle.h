#ifndef HS_POLY_TRIANGLE_H
#define HS_POLY_TRIANGLE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

Poly_Triangle * hs_new_Poly_Triangle_fromIndices(int n1, int n2, int n3);

void hs_delete_Poly_Triangle(Poly_Triangle * triangle);

int hs_Poly_Triangle_value(Poly_Triangle * triangle, int index);

void hs_Poly_Triangle_setValue(Poly_Triangle * triangle, int index, int node);

#ifdef __cplusplus
}
#endif

#endif // HS_POLY_TRIANGLE_H
