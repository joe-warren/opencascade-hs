#include <Standard_Failure.hxx>
#include "hs_Standard_Failure.h"

void hs_delete_Standard_Failure(Standard_Failure * ex){
    delete ex;
}

char * hs_Standard_Failure_getMessageString(Standard_Failure *ex){
    return const_cast<char *>(ex->GetMessageString());
}

void hs_Standard_Failure_catch(void (*inner)(), void(*handler)(Standard_Failure *)){
    try {
        (*inner)();
    } catch (Standard_Failure &ex) {
        (*handler)(&ex);
    }
}