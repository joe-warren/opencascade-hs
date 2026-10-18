#ifndef HS_NCOLLECTION_VEC3_H
#define HS_NCOLLECTION_VEC3_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

VEC3(float) * hs_new_NCollection_Vec3_float(float x, float y, float z);

void hs_delete_NCollection_Vec3_float(VEC3(float) * vec);

#ifdef __cplusplus
}
#endif

#endif // HS_NCOLLECTION_VEC3_H
