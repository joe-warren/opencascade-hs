#include <RWObj_CafWriter.hxx>
#include "hs_RWObj_CafWriter.h"

RWObj_CafWriter * hs_new_RWObj_CafWriter(char * filename){
    return new RWObj_CafWriter(filename);
}

void hs_delete_RWObj_CafWriter(RWObj_CafWriter * writer){
    delete writer;
}

void hs_RWObj_CafWriter_Perform(RWObj_CafWriter * theWriter, Handle(TDocStd_Document) * theDocument, TColStd_IndexedDataMapOfStringString * theFileInfo, Message_ProgressRange * theProgress){
    theWriter->Perform(*theDocument, *theFileInfo, *theProgress);
}