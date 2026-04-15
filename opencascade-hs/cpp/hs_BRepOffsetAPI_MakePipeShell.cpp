#include <BRepOffsetAPI_MakePipeShell.hxx>
#include "hs_BRepOffsetAPI_MakePipeShell.h"

BRepOffsetAPI_MakePipeShell * hs_new_BRepOffsetAPI_MakePipeShell(TopoDS_Wire * spine){
    return new BRepOffsetAPI_MakePipeShell(*spine);
}

void hs_delete_BRepOffsetAPI_MakePipeShell(BRepOffsetAPI_MakePipeShell * builder){
    delete builder;
}

void hs_BRepOffsetAPI_MakePipeShell_setModeFrenet(BRepOffsetAPI_MakePipeShell * builder, bool isFrenet){
    builder->SetMode(static_cast<Standard_Boolean>(isFrenet));
}

void hs_BRepOffsetAPI_MakePipeShell_setDiscreteMode(BRepOffsetAPI_MakePipeShell * builder){
    builder->SetDiscreteMode();
}

void hs_BRepOffsetAPI_MakePipeShell_add(BRepOffsetAPI_MakePipeShell * builder, TopoDS_Wire * profile, bool withContact, bool withCorrection){
    builder->Add(*profile,
                 static_cast<Standard_Boolean>(withContact),
                 static_cast<Standard_Boolean>(withCorrection));
}

bool hs_BRepOffsetAPI_MakePipeShell_makeSolid(BRepOffsetAPI_MakePipeShell * builder){
    return builder->MakeSolid();
}

bool hs_BRepOffsetAPI_MakePipeShell_isDone(BRepOffsetAPI_MakePipeShell * builder){
    return builder->IsDone();
}
