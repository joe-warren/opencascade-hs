#include <XCAFDoc_VisMaterial.hxx>
#include <XCAFDoc_VisMaterialPBR.hxx>
#include "hs_Exception.h"
#include "hs_XCAFDoc_VisMaterial.h"

Handle(XCAFDoc_VisMaterial) * hs_new_XCAFDoc_VisMaterial(){
    return new opencascade::handle<XCAFDoc_VisMaterial>(new XCAFDoc_VisMaterial());
}

void hs_delete_XCAFDoc_VisMaterial(Handle(XCAFDoc_VisMaterial) * material){
    delete material;
}

void hs_XCAFDoc_VisMaterial_setPbrMaterial(
        Handle(XCAFDoc_VisMaterial) * material, XCAFDoc_VisMaterialPBR * pbr,
        HSExceptionType* exType, void ** exPtr
){
    hs_handleExVoid(exType, exPtr, [material, pbr]{
        (*material)->SetPbrMaterial(*pbr);
    });
}

void hs_XCAFDoc_VisMaterial_setAlphaMode(
        Handle(XCAFDoc_VisMaterial) * material, Graphic3d_AlphaMode mode, double cutOff,
        HSExceptionType* exType, void ** exPtr
){
    hs_handleExVoid(exType, exPtr, [material, mode, cutOff]{
        (*material)->SetAlphaMode(mode, (float) cutOff);
    });
}
