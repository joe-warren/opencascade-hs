#ifndef HS_BREPBNDLIB_H
#define HS_BREPBNDLIB_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

void hs_BRepBndLib_add(TopoDS_Shape * shape, Bnd_Box * box, bool useTriangulation);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPBNDLIB_H
