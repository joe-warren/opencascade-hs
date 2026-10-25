#ifndef HS__EXCEPTION__H
#define HS__EXCEPTION__H

#include "hs_types.h"

#ifdef __cplusplus

#include <stdexcept>
#include <Standard_Failure.hxx>
#include <Standard_Version.hxx>
#include <string>

struct HSStandardFailure {
  std::string typeName;
  Standard_Failure failure;
};

inline HSStandardFailure* hs_copyStandardFailure(const Standard_Failure& e) {
#if OCC_VERSION_HEX >= 0x080000
  const char* typeName = e.ExceptionType();
#else
  const char* typeName = e.DynamicType()->Name();
#endif
  return new HSStandardFailure{ typeName ? typeName : "", Standard_Failure(e) };
}

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
    *exPtr = hs_copyStandardFailure(e);
  }
  catch (std::exception &e) {
    *theType = StdExceptionEx;
    std::exception* copy = new std::runtime_error(e.what());
    *exPtr = copy;
  }
  catch (std::exception *e) {
    *theType = StdExceptionEx;
    std::exception* copy = new std::runtime_error(e->what());
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
    *exPtr = hs_copyStandardFailure(e);
  }
  catch (std::exception &e) {
    *theType = StdExceptionEx;
    std::exception* copy = new std::runtime_error(e.what());
    *exPtr = copy;
  }
  catch (std::exception *e) {
    *theType = StdExceptionEx;
    std::exception* copy = new std::runtime_error(e->what());
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

STD_RUNTIME_ERROR * hs_new_runtime_error(char * msg);

void hs_delete_std_exception(STD_EXCEPTION * ex);

char * hs_std_exception_what(STD_EXCEPTION * ex);

void hs_throw_std_exception(STD_EXCEPTION * ex, HSExceptionType* exType, void ** exPtr);

#ifdef __cplusplus
}
#endif


#endif // HS_EXCEPTION_H