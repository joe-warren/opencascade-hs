#include <TopExp_Explorer.hxx>
#include "hs_TopExp_Explorer.h"

TopExp_Explorer * hs_new_TopExp_Explorer(TopoDS_Shape * shape, TopAbs_ShapeEnum toFind){
    return new TopExp_Explorer(*shape, toFind);
}

void hs_delete_TopExp_Explorer(TopExp_Explorer * explorer){
    delete explorer;
}

bool hs_TopExp_Explorer_more(TopExp_Explorer * explorer){
    return explorer->More();
}

void hs_TopExp_Explorer_next(TopExp_Explorer * explorer){
    return explorer->Next();
}

TopoDS_Shape * hs_TopExp_Explorer_value(TopExp_Explorer * explorer){
    return (TopoDS_Shape *) &(explorer->Value());
}