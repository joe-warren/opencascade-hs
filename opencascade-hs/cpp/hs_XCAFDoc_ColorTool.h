#ifndef HS_XCAFDOC_COLORTOOL_H
#define HS_XCAFDOC_COLORTOOL_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

void hs_delete_XCAFDoc_ColorTool(Handle(XCAFDoc_ColorTool) * colorTool);

void hs_XCAFDoc_ColorTool_setColor(
    Handle(XCAFDoc_ColorTool) * colorTool, TDF_Label * label, Quantity_Color * color, XCAFDoc_ColorType colorType,
    HSExceptionType* exType, void ** exPtr
);

bool hs_XCAFDoc_ColorTool_setShapeColor(
    Handle(XCAFDoc_ColorTool) * colorTool, TopoDS_Shape * shape, Quantity_Color * color, XCAFDoc_ColorType colorType,
    HSExceptionType* exType, void ** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_XCAFDOC_COLORTOOL_H
