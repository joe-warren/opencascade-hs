#include <STEPCAFControl_Writer.hxx>
#include <TDocStd_Document.hxx>
#include "hs_Exception.h"
#include "hs_STEPCAFControl_Writer.h"

STEPCAFControl_Writer * hs_new_STEPCAFControl_Writer(){
    return new STEPCAFControl_Writer();
}

void hs_delete_STEPCAFControl_Writer(STEPCAFControl_Writer * writer){
    delete writer;
}

void hs_STEPCAFControl_Writer_setColorMode(STEPCAFControl_Writer * writer, bool colorMode){
    writer->SetColorMode(colorMode);
}

void hs_STEPCAFControl_Writer_setNameMode(STEPCAFControl_Writer * writer, bool nameMode){
    writer->SetNameMode(nameMode);
}

bool hs_STEPCAFControl_Writer_transfer(
        STEPCAFControl_Writer * writer, Handle(TDocStd_Document) * document, STEPControl_StepModelType mode,
        HSExceptionType* exType, void ** exPtr
    ){
    return hs_handleExWithDefault(
        exType,
        exPtr,
        [writer, document, mode]{
            return writer->Transfer(*document, mode);
        },
        false
    );
}

IFSelect_ReturnStatus hs_STEPCAFControl_Writer_write(
        STEPCAFControl_Writer * writer, char * filename,
        HSExceptionType* exType, void ** exPtr
    ){
    return hs_handleExWithDefault(
        exType,
        exPtr,
        [writer, filename]{
            return writer->Write(filename);
        },
        IFSelect_RetError
    );
}
