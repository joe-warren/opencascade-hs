#include <HLRBRep_Algo.hxx>
#include "hs_HLRBRep_Algo.h"
#include <Standard_Handle.hxx>

Handle(HLRBRep_Algo) * hs_new_HLRBRep_Algo(){
    return new Handle(HLRBRep_Algo)(new HLRBRep_Algo());
}

void hs_delete_HLRBRep_Algo(Handle(HLRBRep_Algo) * algo){
    delete algo;
}

void hs_HLRBRep_Algo_projector(Handle(HLRBRep_Algo) * algo, HLRAlgo_Projector * projector){
    (*algo)->Projector(*projector);
}

void hs_HLRBRep_Algo_update(Handle(HLRBRep_Algo) * algo){
    (*algo)->Update();
}

void hs_HLRBRep_Algo_hide(Handle(HLRBRep_Algo) * algo){
    (*algo)->Hide();
}

void hs_HLRBRep_Algo_add(Handle(HLRBRep_Algo) * algo, TopoDS_Shape * shape){
    (*algo)->Add(*shape);
}