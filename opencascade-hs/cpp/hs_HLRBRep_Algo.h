#ifndef HS_HLRBREP_ALGO_H
#define HS_HLRBREP_ALGO_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

Handle(HLRBRep_Algo) * hs_new_HLRBRep_Algo();

void hs_delete_HLRBRep_Algo(Handle(HLRBRep_Algo) * algo);

void hs_HLRBRep_Algo_projector(Handle(HLRBRep_Algo) * algo, HLRAlgo_Projector * projector);

void hs_HLRBRep_Algo_update(Handle(HLRBRep_Algo) * algo);

void hs_HLRBRep_Algo_hide(Handle(HLRBRep_Algo) * algo);

void hs_HLRBRep_Algo_add(Handle(HLRBRep_Algo) * algo, TopoDS_Shape * shape);

#ifdef __cplusplus
}
#endif

#endif // HS_HLRBREP_ALGO_H

