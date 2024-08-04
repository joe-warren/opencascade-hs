#ifndef HS_BREP_TOOL_H
#define HS_BREP_TOOL_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

Handle(Geom_Curve) * hs_BRep_Tool_curve(TopoDS_Edge * edge);

double hs_BRep_Tool_curveParamFirst(TopoDS_Edge * edge);

double hs_BRep_Tool_curveParamLast(TopoDS_Edge * edge);

gp_Pnt * hs_BRep_Tool_pnt(TopoDS_Vertex * vertex);

Handle(Poly_Triangulation) * hs_BRep_Tool_triangulation(TopoDS_Face * face, TopLoc_Location * loc);

#ifdef __cplusplus
}
#endif

#endif // HS_BREP_TOOL_H
