#include <Standard_Failure.hxx>
#include <Standard_Version.hxx>
#include "hs_Standard_Failure.h"

void hs_delete_Standard_Failure(Standard_Failure *ex) {
    delete ex;
}

char * hs_Standard_Failure_GetMessageString(Standard_Failure *ex) {
#if OCC_VERSION_HEX >= 0x080000
    return const_cast<char *>(ex->what());
#else
    return const_cast<char *>(ex->GetMessageString());
#endif
}

char * hs_Standard_Failure_GetStackString(Standard_Failure *ex) {
    return const_cast<char *>(ex->GetStackString());
}