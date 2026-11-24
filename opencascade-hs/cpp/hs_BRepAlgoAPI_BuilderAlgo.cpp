#include <BRepAlgoAPI_BuilderAlgo.hxx>
#include "hs_Exception.h"
#include "hs_BRepAlgoAPI_BuilderAlgo.h"

void hs_BRepAlgoAPI_BuilderAlgo_setNonDestructive(BRepAlgoAPI_BuilderAlgo * builder, bool nonDestructive){
    builder->SetNonDestructive(nonDestructive);
}

void hs_BRepAlgoAPI_BuilderAlgo_setArguments(
    BRepAlgoAPI_BuilderAlgo * builder, 
    LIST(TopoDS_Shape) * arguments, 
    HSExceptionType* exType,
    void** exPtr
){
    return hs_handleExVoid(
        exType,
        exPtr,
        [builder, arguments]{
        builder->SetArguments(*arguments);
    });
}