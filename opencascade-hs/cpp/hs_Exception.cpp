#include <Standard_Failure.hxx>
#include "hs_types.h"
#include "hs_Exception.h"

STD_RUNTIME_ERROR * hs_new_runtime_error(char * msg){
    return new std::runtime_error(const_cast<const char *>(msg));
}

void hs_delete_std_exception(STD_EXCEPTION * ex){
    delete ex;
}

void hs_throw_std_exception(STD_EXCEPTION * ex, HSExceptionType* exType, void ** exPtr){
    hs_handleExVoid( 
        exType,
        exPtr,
        [ex]{
            throw ex;
        });
}

char * hs_std_exception_what(STD_EXCEPTION * ex){
    return const_cast<char *>(ex->what());
}