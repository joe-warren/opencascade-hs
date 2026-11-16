#include <RWObj_CafWriter.hxx>
#include <NCollection_IndexedDataMap.hxx>
#include <TCollection_AsciiString.hxx>
#include "hs_Exception.h"
#include "hs_RWObj_CafWriter.h"

RWObj_CafWriter * hs_new_RWObj_CafWriter(char * filename){
    return new RWObj_CafWriter(filename);
}

void hs_delete_RWObj_CafWriter(RWObj_CafWriter * writer){
    delete writer;
}

bool hs_RWObj_CafWriter_Perform(
    RWObj_CafWriter * theWriter,
    Handle(TDocStd_Document) * theDocument,
    INDEXED_DATA_MAP(TCollection_AsciiString, TCollection_AsciiString, DEFAULT_HASHER(TCollection_AsciiString)) * theFileInfo,
    Message_ProgressRange * theProgress,
    HSExceptionType* exType, void ** exPtr
){
    return hs_handleExWithDefault(
        exType,
        exPtr,
        [theWriter, theDocument, theFileInfo, theProgress]{
            return theWriter->Perform(*theDocument, *theFileInfo, *theProgress);
        }
        , false
    );
}