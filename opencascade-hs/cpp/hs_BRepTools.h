#ifndef HS_BREPTOOLS_H
#define HS_BREPTOOLS_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

TopoDS_Wire * hs_BRepTools_OuterWire(
        TopoDS_Face * face,
        HSExceptionType* exType, void ** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPTOOLS_H
