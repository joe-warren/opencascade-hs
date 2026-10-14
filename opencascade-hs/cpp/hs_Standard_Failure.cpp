#include <Standard_Failure.hxx>
#include <Standard_Version.hxx>
#include "hs_Exception.h"
#include "hs_Standard_Failure.h"

void hs_delete_Standard_Failure(HS_STANDARD_FAILURE *ex) {
    delete ex;
}

char * hs_Standard_Failure_GetTypeName(HS_STANDARD_FAILURE *ex) {
    return const_cast<char *>(ex->typeName.c_str());
}

char * hs_Standard_Failure_GetMessageString(HS_STANDARD_FAILURE *ex) {
#if OCC_VERSION_HEX >= 0x080000
    return const_cast<char *>(ex->failure.what());
#else
    return const_cast<char *>(ex->failure.GetMessageString());
#endif
}

char * hs_Standard_Failure_GetStackString(HS_STANDARD_FAILURE *ex) {
    return const_cast<char *>(ex->failure.GetStackString());
}
