#ifndef HS_BOPALGO_TOOLS_H
#define HS_BOPALGO_TOOLS_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

TopoDS_Shape * hs_BOPAlgo_Tools_EdgesToWires(TopoDS_Shape * edges, bool shared, double angTol, int * status, HSExceptionType* exType, void ** exPtr);

TopoDS_Shape * hs_BOPAlgo_Tools_WiresToFaces(TopoDS_Shape * wires, double angTol, bool * success, HSExceptionType* exType, void ** exPtr);

#ifdef __cplusplus
}
#endif

#endif // HS_BOPALGO_TOOLS_H
