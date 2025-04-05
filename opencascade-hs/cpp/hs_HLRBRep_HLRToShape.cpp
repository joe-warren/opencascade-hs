#include <HLRBRep_HLRToShape.hxx>
#include "hs_HLRBRep_HLRToShape.h"

HLRBRep_HLRToShape * hs_new_HLRBRep_HLRToShape_fromHandleAlgo(Handle(HLRBRep_Algo) * algo){
    return new HLRBRep_HLRToShape(*algo);
}

void hs_delete_HLRBRep_HLRToShape(HLRBRep_HLRToShape *hlrToShape){
    delete hlrToShape;
}

TopoDS_Shape * hs_HLRBRep_HLRToShape_compoundOfEdges(HLRBRep_HLRToShape * toShape, HLRBRep_TypeOfResultingEdge edgeType, bool visible, bool in3d){
    return new TopoDS_Shape(toShape->CompoundOfEdges(edgeType, visible, in3d));
}