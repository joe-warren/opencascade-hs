#include <TColStd_IndexedDataMapOfStringString.hxx>
#include "hs_TColStd_IndexedDataMapOfStringString.h"

TColStd_IndexedDataMapOfStringString * hs_new_TColStd_IndexedDataMapOfStringString(){
    return new TColStd_IndexedDataMapOfStringString();
}

void hs_delete_TColStd_IndexedDataMapOfStringString(TColStd_IndexedDataMapOfStringString * theMap){
    delete theMap;
}