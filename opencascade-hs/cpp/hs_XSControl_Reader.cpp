#include <XSControl_Reader.hxx>
#include "hs_Exception.h"
#include "hs_XSControl_Reader.h"

IFSelect_ReturnStatus hs_XSControl_Reader_readFile(
    XSControl_Reader * reader, char * filename,
    HSExceptionType* exType, void ** exPtr
    ){
    return hs_handleExWithDefault(
        exType,
        exPtr,
        [reader, filename]{
            return reader->ReadFile(filename);
        },
        IFSelect_RetError
    );
}

bool hs_XSControl_Reader_transferRoots(
    XSControl_Reader * reader,
    HSExceptionType* exType, void ** exPtr
    ){
        
    return hs_handleExWithDefault(
        exType,
        exPtr,
        [reader]{
            return reader->TransferRoots();
        },
        false
    );
}

TopoDS_Shape * hs_XSControl_Reader_oneShape(
    XSControl_Reader * reader,
    HSExceptionType* exType, void ** exPtr
    ){
    return hs_handleEx(
        exType,
        exPtr,
        [reader]{
            return new TopoDS_Shape(reader->OneShape());
        }
    );
}