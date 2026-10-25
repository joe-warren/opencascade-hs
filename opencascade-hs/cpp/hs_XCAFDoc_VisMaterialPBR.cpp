#include <XCAFDoc_VisMaterialPBR.hxx>
#include <Quantity_ColorRGBA.hxx>
#include <NCollection_Vec3.hxx>
#include "hs_XCAFDoc_VisMaterialPBR.h"

XCAFDoc_VisMaterialPBR * hs_new_XCAFDoc_VisMaterialPBR(){
    return new XCAFDoc_VisMaterialPBR();
}

void hs_delete_XCAFDoc_VisMaterialPBR(XCAFDoc_VisMaterialPBR * pbr){
    delete pbr;
}

void hs_XCAFDoc_VisMaterialPBR_setBaseColor(XCAFDoc_VisMaterialPBR * pbr, Quantity_ColorRGBA * color){
    pbr->BaseColor = *color;
}

void hs_XCAFDoc_VisMaterialPBR_setMetallic(XCAFDoc_VisMaterialPBR * pbr, double metallic){
    pbr->Metallic = (float) metallic;
}

void hs_XCAFDoc_VisMaterialPBR_setRoughness(XCAFDoc_VisMaterialPBR * pbr, double roughness){
    pbr->Roughness = (float) roughness;
}

void hs_XCAFDoc_VisMaterialPBR_setEmissiveFactor(XCAFDoc_VisMaterialPBR * pbr, VEC3(float) * factor){
    pbr->EmissiveFactor = *factor;
}

void hs_XCAFDoc_VisMaterialPBR_setRefractionIndex(XCAFDoc_VisMaterialPBR * pbr, double refractionIndex){
    pbr->RefractionIndex = (float) refractionIndex;
}
