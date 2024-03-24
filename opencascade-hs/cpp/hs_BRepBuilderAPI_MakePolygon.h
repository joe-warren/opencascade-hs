#ifndef HS_BREPBUILDERAPI_MAKEPOLYGON_H
#define HS_BREPBUILDERAPI_MAKEPOLYGON_H

#include "hs_types.h"

#ifdef __cplusplus
    extern "C" {
#endif

TopoDS_Wire * hs_BRepBuilderAPI_MakePolygon_from3Pnts(gp_Pnt * n1, gp_Pnt *n2, gp_Pnt * n3, bool close);

#ifdef __cplusplus
    }
#endif

#endif // HS_BREPBUILDERAPI_MAKEPOLYGON_H
