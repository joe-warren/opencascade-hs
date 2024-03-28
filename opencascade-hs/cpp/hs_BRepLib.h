#ifndef HS_BREPLIB_H
#define HS_BREPLIB_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

bool hs_BRepLib_orientClosedSolid(TopoDS_Solid * solid);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPLIB_H
