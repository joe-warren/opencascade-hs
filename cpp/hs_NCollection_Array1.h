#ifndef HS_NCOLLECTION_ARRAY1_H
#define HS_NCOLLECTION_ARRAY1_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

ARRAY_1(gp_Pnt) * hs_new_NCollection_Array1_gp_Pnt(int lower, int upper);

void hs_NCollection_Array1_gp_Pnt_setValue(ARRAY_1(gp_Pnt) * arr, int index, gp_Pnt * value);

void hs_delete_NCollection_Array1_gp_Pnt(ARRAY_1(gp_Pnt) * arr);

#ifdef __cplusplus
}
#endif

#endif // HS_NCOLLECTION_ARRAY1_H
