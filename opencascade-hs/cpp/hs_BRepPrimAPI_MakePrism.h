#ifndef HS_BREPPRIMAPI_MAKEPRISM_H
#define HS_BREPPRIMAPI_MAKEPRISM_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

TopoDS_Shape * hs_BRepPrimAPI_MakePrism_fromVec(TopoDS_Shape * shape, gp_Vec * vec, bool copy, bool canonize);

TopoDS_Shape * hs_BRepPrimAPI_MakePrism_fromDir(TopoDS_Shape * shape, gp_Dir * dir, bool inf, bool copy, bool canonize);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPPRIMAPI_MAKEPRISM_H
