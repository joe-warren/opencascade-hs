#include <HLRBRep_Algo.hxx>
#include "hs_HLRBRep_Algo.h"
#include <Standard_Handle.hxx>

HLRBRep_Algo * hs_new_HLRBRep_Algo(){
    return new HLRBRep_Algo();
}

void hs_delete_HLRBRep_Algo(HLRBRep_Algo * algo){
    delete algo;
}

Handle(HLRBRep_Algo) * hs_HLRBRep_Algo_toHandle(HLRBRep_Algo *algo){
    return new Handle(HLRBRep_Algo)(new HLRBRep_Algo(algo));
}

void hs_delete_Handle_HLRBRep_Algo(Handle(HLRBRep_Algo) *handleAlgo){
    delete handleAlgo;
}

void hs_HLRBRep_Algo_projector(HLRBRep_Algo * algo, HLRAlgo_Projector * projector){
    algo->Projector(*projector);
}

void hs_HLRBRep_Algo_update(HLRBRep_Algo * algo){
    algo->Update();
}

void hs_HLRBRep_Algo_hide(HLRBRep_Algo * algo){
    algo->Hide();
}

void hs_HLRBRep_Algo_add(HLRBRep_Algo * algo, TopoDS_Shape * shape){
    algo->Add(*shape);
}