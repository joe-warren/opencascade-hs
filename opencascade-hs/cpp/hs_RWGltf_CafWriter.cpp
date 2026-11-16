#include <RWGltf_CafWriter.hxx>
#include <NCollection_IndexedDataMap.hxx>
#include <TCollection_AsciiString.hxx>
#include "hs_Exception.h"
#include "hs_RWGltf_CafWriter.h"

RWGltf_CafWriter * hs_new_RWGltf_CafWriter(char * theFile, bool isBinary){
    return new RWGltf_CafWriter(theFile, isBinary);
}

void hs_delete_RWGltf_CafWriter(RWGltf_CafWriter * theWriter){
    delete theWriter;   
}

bool hs_RWGltf_CafWriter_Perform(
    RWGltf_CafWriter * theWriter,
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