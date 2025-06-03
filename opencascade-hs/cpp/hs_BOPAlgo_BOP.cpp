#include <BOPAlgo_BOP.hxx>
#include "hs_BOPAlgo_BOP.h"


BOPAlgo_BOP * hs_new_BOPAlgo_BOP(){
    return new BOPAlgo_BOP();
}

void hs_delete_BOPAlgo_BOP(BOPAlgo_BOP * bop){
    delete bop;
}

void hs_BOPAlgo_BOP_AddTool(BOPAlgo_BOP *bop, TopoDS_Shape * tool){
    bop->AddTool(*tool);
}

void hs_BOPAlgo_BOP_SetOperation(BOPAlgo_BOP *bop, BOPAlgo_Operation operation){
    bop->SetOperation(operation);
}
