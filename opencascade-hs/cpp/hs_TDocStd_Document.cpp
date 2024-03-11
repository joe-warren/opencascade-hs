#include <TDocStd_Document.hxx>
#include <XCAFDoc_ShapeTool.hxx>
#include "hs_TDocStd_Document.h"

Handle(TDocStd_Document) * hs_new_TDocStd_Document(char * storageFormat){
    return new opencascade::handle<TDocStd_Document>(new TDocStd_Document(storageFormat));
}

void hs_delete_TDocStd_Document(Handle(TDocStd_Document) * theDocument){
    delete theDocument;
}

TDF_Label * hs_TDocStd_Document_main(Handle(TDocStd_Document) *theDocument){
    return new TDF_Label(theDocument->get()->Main());
}