#ifndef HS_BOPALGO_TOOLS_H
#define HS_BOPALGO_TOOLS_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

int hs_BOPAlgo_Tools_EdgesToWires(TopoDS_Shape * edges, TopoDS_Shape * wires, bool shared, double angTol, HSExceptionType* exType, void ** exPtr);

bool hs_BOPAlgo_Tools_WiresToFaces(TopoDS_Shape * wires, TopoDS_Shape * faces, double angTol, HSExceptionType* exType, void ** exPtr);

#ifdef __cplusplus
}
#endif

#endif // HS_BOPALGO_TOOLS_H
