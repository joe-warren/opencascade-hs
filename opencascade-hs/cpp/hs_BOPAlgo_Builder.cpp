#include <BOPAlgo_Builder.hxx>
#include "hs_BOPAlgo_Builder.h"

BOPAlgo_Builder * hs_new_BOPAlgo_Builder(){
    return new BOPAlgo_Builder();
}

void hs_delete_BOPAlgo_Builder(BOPAlgo_Builder * builder){
    delete builder;
}

void hs_BOPAlgo_Builder_AddArgument(BOPAlgo_Builder * builder, TopoDS_Shape * shape){
    builder->AddArgument(*shape);
}

TopoDS_Shape * hs_BOPAlgo_Builder_Shape(BOPAlgo_Builder * builder){
    return new TopoDS_Shape(builder->Shape());
}

void hs_BOPAlgo_Builder_SetRunParallel(BOPAlgo_Builder * builder, bool runParallel){
    builder->SetRunParallel(runParallel);
}

