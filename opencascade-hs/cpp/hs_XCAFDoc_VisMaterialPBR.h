#ifndef HS_XCAFDOC_VISMATERIALPBR_H
#define HS_XCAFDOC_VISMATERIALPBR_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

XCAFDoc_VisMaterialPBR * hs_new_XCAFDoc_VisMaterialPBR();

void hs_delete_XCAFDoc_VisMaterialPBR(XCAFDoc_VisMaterialPBR * pbr);

void hs_XCAFDoc_VisMaterialPBR_setBaseColor(XCAFDoc_VisMaterialPBR * pbr, double r, double g, double b, double a);

void hs_XCAFDoc_VisMaterialPBR_setMetallic(XCAFDoc_VisMaterialPBR * pbr, double metallic);

void hs_XCAFDoc_VisMaterialPBR_setRoughness(XCAFDoc_VisMaterialPBR * pbr, double roughness);

void hs_XCAFDoc_VisMaterialPBR_setEmissiveFactor(XCAFDoc_VisMaterialPBR * pbr, double r, double g, double b);

void hs_XCAFDoc_VisMaterialPBR_setRefractionIndex(XCAFDoc_VisMaterialPBR * pbr, double refractionIndex);

#ifdef __cplusplus
}
#endif

#endif // HS_XCAFDOC_VISMATERIALPBR_H
