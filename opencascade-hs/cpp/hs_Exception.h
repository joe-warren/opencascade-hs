#ifndef HS__EXCEPTION__H
#define HS__EXCEPTION__H

#include "hs_types.h"

#ifdef __cplusplus

enum HSExceptionType {
    NoException = 0,
    StandardFailureEx = 1,
    StdExceptionEx = 2,
    OtherEx = 3
};

template <typename T, typename Function, typename ... Args>
auto hs_handleEx(HSExceptionType* theType, void ** exPtr, T* t, Function f, Args&& ... args) {
  try {
    *theType = NoException;
    return (t->*f)(std::forward<Args>(args)...);
  }
  catch (Standard_Failure &e) {
    *theType = StandardFailureEx;
    *exPtr = new Standard_Failure(e);
  }
  catch (std::exception &e) {
    *theType = StdExceptionEx;
    *exPtr = new std::exception(e);
  }
  catch(...) {
    *theType = OtherEx;
  }
}

#endif // __cplusplus


#ifdef __cplusplus
extern "C" {
#endif

void hs_delete_std_exception(STD_EXCEPTION * ex);

char * hs_std_exception_what(STD_EXCEPTION * ex);

#ifdef __cplusplus
}
#endif


#endif // HS_EXCEPTION_H