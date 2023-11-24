#ifndef HS_BREP_TOOL_H
#define HS_BREP_TOOL_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

Handle(Geom_Curve) * hs_BRep_Tool_curve(TopoDS_Edge * edge, double first, double last);

#ifdef __cplusplus
}
#endif

#endif // HS_BREP_TOOL_H
