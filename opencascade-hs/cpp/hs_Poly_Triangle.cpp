#include <Poly_Triangle.hxx>
#include "hs_Poly_Triangle.h"

Poly_Triangle * hs_new_Poly_Triangle_fromIndices(int n1, int n2, int n3){
    return new Poly_Triangle(n1, n2, n3);
}

void hs_delete_Poly_Triangle(Poly_Triangle * triangle){
    delete triangle;
}

int hs_Poly_Triangle_value(Poly_Triangle * triangle, int index){
    return triangle->Value(index);
}

void hs_Poly_Triangle_setValue(Poly_Triangle * triangle, int index, int node){
    triangle->Set(index, node);
}