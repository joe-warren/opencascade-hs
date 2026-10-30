#include <BRepOffsetAPI_MakePipeShell.hxx>
#include "hs_Exception.h"
#include "hs_BRepOffsetAPI_MakePipeShell.h"

BRepOffsetAPI_MakePipeShell * hs_new_BRepOffsetAPI_MakePipeShell(
        TopoDS_Wire * spine,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [spine]{
            return new BRepOffsetAPI_MakePipeShell(*spine);
        });
}

void hs_delete_BRepOffsetAPI_MakePipeShell(BRepOffsetAPI_MakePipeShell * builder){
    delete builder;
}

void hs_BRepOffsetAPI_MakePipeShell_setModeFrenet(BRepOffsetAPI_MakePipeShell * builder, bool isFrenet){
    builder->SetMode(static_cast<bool>(isFrenet));
}

void hs_BRepOffsetAPI_MakePipeShell_setDiscreteMode(BRepOffsetAPI_MakePipeShell * builder){
    builder->SetDiscreteMode();
}

void hs_BRepOffsetAPI_MakePipeShell_add(
        BRepOffsetAPI_MakePipeShell * builder,
        TopoDS_Wire * profile,
        bool withContact,
        bool withCorrection,
        HSExceptionType* exType,
        void** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [builder, profile, withContact, withCorrection]{
            builder->Add(*profile,
                         static_cast<bool>(withContact),
                         static_cast<bool>(withCorrection));
        });
}

bool hs_BRepOffsetAPI_MakePipeShell_makeSolid(
        BRepOffsetAPI_MakePipeShell * builder,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleExWithDefault(
        exType,
        exPtr,
        [builder]{
            return static_cast<bool>(builder->MakeSolid());
        },
        false);
}

bool hs_BRepOffsetAPI_MakePipeShell_isDone(BRepOffsetAPI_MakePipeShell * builder){
    return builder->IsDone();
}
