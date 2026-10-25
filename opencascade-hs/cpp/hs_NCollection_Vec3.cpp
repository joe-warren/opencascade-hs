#include <NCollection_Vec3.hxx>
#include "hs_NCollection_Vec3.h"

VEC3(float) * hs_new_NCollection_Vec3_float(float x, float y, float z){
    return new NCollection_Vec3<float>(x, y, z);
}

void hs_delete_NCollection_Vec3_float(VEC3(float) * vec){
    delete vec;
}
