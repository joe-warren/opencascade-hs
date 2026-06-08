#ifndef HS__EXCEPTION__H
#define HS__EXCEPTION__H

#include "hs_types.h"

#ifdef __cplusplus

#include <stdexcept>

enum HSExceptionType {
    NoException = 0,
    StandardFailureEx = 1,
    StdExceptionEx = 2,
    OtherEx = 3
};

template <typename T>
void hs_handleExVoid(HSExceptionType* theType, void ** exPtr, T&& f) {
  try {
    *theType = NoException;
    f();
  }
  catch (Standard_Failure &e) {
    *theType = StandardFailureEx;
    *exPtr = new Standard_Failure(e);
  }
  catch (std::exception &e) {
    *theType = StdExceptionEx;
    std::exception* copy = new std::runtime_error(e.what());
    *exPtr = copy;
  }
  catch(...) {
    *theType = OtherEx;
  }
}

template <typename T>
auto hs_handleExWithDefault(HSExceptionType* theType, void ** exPtr, T&& f, decltype(f()) fallback) {
  try {
    *theType = NoException;
    return f();
  }
  catch (Standard_Failure &e) {
    *theType = StandardFailureEx;
    *exPtr = new Standard_Failure(e);
  }
  catch (std::exception &e) {
    *theType = StdExceptionEx;
    std::exception* copy = new std::runtime_error(e.what());
    *exPtr = copy;
  }
  catch(...) {
    *theType = OtherEx;
  }
  return fallback;
}

template <typename T>
auto hs_handleEx(HSExceptionType* theType, void ** exPtr, T&& f) {
  return hs_handleExWithDefault(
    theType, 
    exPtr, 
    f,
    static_cast<decltype(f())>(nullptr)
  );
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