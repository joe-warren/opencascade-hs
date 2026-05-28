#include <Standard_Failure.hxx>
#include "hs_Standard_Failure.h"

void hs_delete_Standard_Failure(Standard_Failure *ex) {
    delete ex;
}

char * hs_Standard_Failure_GetStackString(Standard_Failure *ex) {
    return const_cast<char *>(ex->GetStackString());
}