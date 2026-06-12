#ifndef HS_HLRBREP_HLRTOSHAPE_H
#define HS_HLRBREP_HLRTOSHAPE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

HLRBRep_HLRToShape * hs_new_HLRBRep_HLRToShape_fromHandleAlgo(
    Handle(HLRBRep_Algo) * algo,
    HSExceptionType* exType, void ** exPtr
);

void hs_delete_HLRBRep_HLRToShape(HLRBRep_HLRToShape *hlrToShape);

TopoDS_Shape * hs_HLRBRep_HLRToShape_compoundOfEdges(
    HLRBRep_HLRToShape * toShape, HLRBRep_TypeOfResultingEdge edgeType, bool visible, bool in3d,
    HSExceptionType* exType, void ** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_HLRBREP_HLRTOSHAPE_H

