#ifndef HS_HLR_ALGO_PROJECTOR_H
#define HS_HLR_ALGO_PROJECTOR_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

HLRAlgo_Projector * hs_new_HLRAlgo_Projector_fromAx2(gp_Ax2* axis);

void hs_delete_HLRAlgo_Projector(HLRAlgo_Projector* projector);
#ifdef __cplusplus
}
#endif

#endif // HS_HLR_ALGO_PROJECTOR_H

