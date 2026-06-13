#include <RWObj_CafWriter.hxx>
#include <TColStd_IndexedDataMapOfStringString.hxx>
#include "hs_Exception.h"
#include "hs_RWObj_CafWriter.h"

RWObj_CafWriter * hs_new_RWObj_CafWriter(char * filename){
    return new RWObj_CafWriter(filename);
}

void hs_delete_RWObj_CafWriter(RWObj_CafWriter * writer){
    delete writer;
}

void hs_RWObj_CafWriter_Perform(
    RWObj_CafWriter * theWriter, Handle(TDocStd_Document) * theDocument, TColStd_IndexedDataMapOfStringString * theFileInfo, Message_ProgressRange * theProgress,
    HSExceptionType* exType, void ** exPtr
){
    hs_handleExVoid(
        exType,
        exPtr,
        [theWriter, theDocument, theFileInfo, theProgress]{
            theWriter->Perform(*theDocument, *theFileInfo, *theProgress);
        }
    );
}