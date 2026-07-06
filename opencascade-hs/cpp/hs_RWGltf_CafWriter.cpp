#include <RWGltf_CafWriter.hxx>
#include <TColStd_IndexedDataMapOfStringString.hxx>
#include "hs_Exception.h"
#include "hs_RWGltf_CafWriter.h"

RWGltf_CafWriter * hs_new_RWGltf_CafWriter(char * theFile, bool isBinary){
    return new RWGltf_CafWriter(theFile, isBinary);
}

void hs_delete_RWGltf_CafWriter(RWGltf_CafWriter * theWriter){
    delete theWriter;   
}

void hs_RWGltf_CafWriter_Perform(
    RWGltf_CafWriter * theWriter, Handle(TDocStd_Document) * theDocument, TColStd_IndexedDataMapOfStringString * theFileInfo, Message_ProgressRange * theProgress,
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