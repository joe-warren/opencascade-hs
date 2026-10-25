#ifndef HS_XCAFDOC_VISMATERIALTOOL_H
#define HS_XCAFDOC_VISMATERIALTOOL_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

void hs_delete_XCAFDoc_VisMaterialTool(Handle(XCAFDoc_VisMaterialTool) * materialTool);

TDF_Label * hs_XCAFDoc_VisMaterialTool_addMaterial(
    Handle(XCAFDoc_VisMaterialTool) * materialTool, Handle(XCAFDoc_VisMaterial) * material, char * name,
    HSExceptionType* exType, void ** exPtr
);

void hs_XCAFDoc_VisMaterialTool_setShapeMaterial(
    Handle(XCAFDoc_VisMaterialTool) * materialTool, TDF_Label * shapeLabel, TDF_Label * materialLabel,
    HSExceptionType* exType, void ** exPtr
);

bool hs_XCAFDoc_VisMaterialTool_setShapeMaterialFromShape(
    Handle(XCAFDoc_VisMaterialTool) * materialTool, TopoDS_Shape * shape, TDF_Label * materialLabel,
    HSExceptionType* exType, void ** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_XCAFDOC_VISMATERIALTOOL_H
