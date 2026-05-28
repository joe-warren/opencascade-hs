#include <Standard_Failure.hxx>
#include "hs_types.h"
#include "hs_Exception.h"

void hs_delete_std_exception(STD_EXCEPTION * ex){
    delete ex;
}

char * hs_std_exception_what(STD_EXCEPTION * ex){
    return const_cast<char *>(ex->what());
}