#ifndef HS_BREPPRIMAPI_MAKECONE_H
#define HS_BREPPRIMAPI_MAKECONE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

TopoDS_Solid * hs_BRepPrimAPI_MakeCone_fromTwoRadiiAndHeight(double r1, double r2, double h);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPPRIMAPI_MAKECONE_H
