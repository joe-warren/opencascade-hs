#include <HLRAlgo_Projector.hxx>
#include <gp_Trsf.hxx>
#include "hs_HLRAlgo_Projector.h"

HLRAlgo_Projector * hs_new_HLRAlgo_Projector_fromAx2(gp_Ax2* axis){
    return new HLRAlgo_Projector(*axis);
}

void hs_delete_HLRAlgo_Projector(HLRAlgo_Projector* projector){
    delete projector;
}

gp_Trsf * hs_HLRAlgo_Projector_Transformation(HLRAlgo_Projector* projector){
    return new gp_Trsf(projector->Transformation());
}

gp_Trsf * hs_HLRAlgo_Projector_InvertedTransformation(HLRAlgo_Projector* projector){
    return new gp_Trsf(projector->InvertedTransformation());
}
