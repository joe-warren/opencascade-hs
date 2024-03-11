#ifndef HS_XCAFDOC_SHAPETOOL_H
#define HS_XCAFDOC_SHAPETOOL_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

void hs_delete_XCAFDoc_ShapeTool(Handle(XCAFDoc_ShapeTool) * shapeTool);

TDF_Label * hs_XCAFDoc_ShapeTool_addShape(Handle(XCAFDoc_ShapeTool) *shapeTool, TopoDS_Shape *theShape, bool makeAssembly, bool makePrepare);

#ifdef __cplusplus
}
#endif

#endif // HS_XCAFDOC_SHAPETOOL_H