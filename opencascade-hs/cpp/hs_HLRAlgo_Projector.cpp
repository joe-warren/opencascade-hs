#include <HLRAlgo_Projector.hxx>
#include "hs_HLRAlgo_Projector.h"

HLRAlgo_Projector * hs_new_HLRAlgo_Projector_fromAx2(gp_Ax2* axis){
    return new HLRAlgo_Projector(*axis);
}

void hs_delete_HLRAlgo_Projector(HLRAlgo_Projector* projector){
    delete projector;
}