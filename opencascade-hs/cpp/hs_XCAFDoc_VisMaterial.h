#ifndef HS_XCAFDOC_VISMATERIAL_H
#define HS_XCAFDOC_VISMATERIAL_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

Handle(XCAFDoc_VisMaterial) * hs_new_XCAFDoc_VisMaterial();

void hs_delete_XCAFDoc_VisMaterial(Handle(XCAFDoc_VisMaterial) * material);

void hs_XCAFDoc_VisMaterial_setPbrMaterial(
    Handle(XCAFDoc_VisMaterial) * material, XCAFDoc_VisMaterialPBR * pbr,
    HSExceptionType* exType, void ** exPtr
);

void hs_XCAFDoc_VisMaterial_setAlphaMode(
    Handle(XCAFDoc_VisMaterial) * material, Graphic3d_AlphaMode mode, double cutOff,
    HSExceptionType* exType, void ** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_XCAFDOC_VISMATERIAL_H
