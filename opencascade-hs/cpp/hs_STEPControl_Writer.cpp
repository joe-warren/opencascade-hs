#include <STEPControl_Writer.hxx>
#include "hs_Exception.h"
#include "hs_STEPControl_Writer.h"

STEPControl_Writer * hs_new_STEPControl_Writer(){
    return new STEPControl_Writer();
}

void hs_delete_STEPControl_Writer(STEPControl_Writer * writer){
    delete writer;
}

void hs_STEPControl_Writer_setTolerance(STEPControl_Writer * writer, double tolerance){
    writer->SetTolerance(tolerance);
}

void hs_STEPControl_Writer_unsetTolerance(STEPControl_Writer * writer){
    writer->UnsetTolerance();
}

IFSelect_ReturnStatus hs_STEPControl_Writer_transfer(
    STEPControl_Writer * writer, TopoDS_Shape * shape, STEPControl_StepModelType mode, bool compgraph,
    HSExceptionType* exType, void ** exPtr
){
    return hs_handleExWithDefault(
        exType,
        exPtr,
        [writer, shape, mode, compgraph]{
            return writer->Transfer(*shape, mode, compgraph);
        },
        IFSelect_RetError
    );
}

IFSelect_ReturnStatus hs_STEPControl_Writer_write(
    STEPControl_Writer* writer, char* filename,
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
