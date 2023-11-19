#include <NCollection_Array1.hxx>
#include <gp_Pnt.hxx>
#include "hs_NCollection_Array1.h"

ARRAY_1(gp_Pnt) * hs_new_NCollection_Array1_gp_Pnt(int lower, int upper){
    return new NCollection_Array1<gp_Pnt>(lower, upper);
}

void hs_NCollection_Array1_gp_Pnt_setValue(ARRAY_1(gp_Pnt) * arr, int index, gp_Pnt * value){
    arr->SetValue(index, *value);
}

void hs_delete_NCollection_Array1_gp_Pnt(ARRAY_1(gp_Pnt) * arr){
    delete arr;
}