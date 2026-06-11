#include <TDocStd_Document.hxx>
#include <XCAFDoc_ShapeTool.hxx>
#include "hs_Exception.h"
#include "hs_TDocStd_Document.h"

Handle(TDocStd_Document) * hs_new_TDocStd_Document(
        char * storageFormat,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(exType, exPtr, [storageFormat]{
        return new opencascade::handle<TDocStd_Document>(new TDocStd_Document(storageFormat));
    });
}

void hs_delete_TDocStd_Document(Handle(TDocStd_Document) * theDocument){
    delete theDocument;
}

TDF_Label * hs_TDocStd_Document_main(
        Handle(TDocStd_Document) *theDocument,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(exType, exPtr, [theDocument]{
        return new TDF_Label(theDocument->get()->Main());
    });
}