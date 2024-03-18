#include <TopoDS_Builder.hxx>
#include "hs_TopoDS_Builder.h"

TopoDS_Builder * hs_new_TopoDS_Builder(void){
    return new TopoDS_Builder();
}

void hs_delete_TopoDS_Builder(TopoDS_Builder * builder){
    delete builder;
}


void hs_TopoDS_Builder_makeWire(TopoDS_Builder * builder, TopoDS_Wire * wire){
    builder->MakeWire(*wire);
}


void hs_TopoDS_Builder_makeShell(TopoDS_Builder * builder, TopoDS_Shell * shell){
    builder->MakeShell(*shell);
}

void hs_TopoDS_Builder_makeSolid(TopoDS_Builder * builder, TopoDS_Solid * solid){
    builder->MakeSolid(*solid);
}

void hs_TopoDS_Builder_makeCompSolid(TopoDS_Builder * builder, TopoDS_CompSolid * solid){
    builder->MakeCompSolid(*solid);
}

void hs_TopoDS_Builder_makeCompound(TopoDS_Builder * builder, TopoDS_Compound * compound){
    builder->MakeCompound(*compound);
}

void hs_TopoDS_Builder_add(TopoDS_Builder * builder, TopoDS_Shape * s, TopoDS_Shape * c){
    builder->Add(*s, *c);
}

void hs_TopoDS_Builder_remove(TopoDS_Builder * builder, TopoDS_Shape * s, TopoDS_Shape * c){
    builder->Remove(*s, *c);
}




