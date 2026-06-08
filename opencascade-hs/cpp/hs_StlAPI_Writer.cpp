#include <StlAPI_Reader.hxx>
#include <Standard_Failure.hxx>
#include "hs_Exception.h"
#include "hs_StlAPI_Reader.h"

StlAPI_Reader * hs_new_StlAPI_Reader(){
    return new StlAPI_Reader();
}

void hs_delete_StlAPI_Reader(StlAPI_Reader * reader){
    delete reader;
}

bool hs_StlAPI_Reader_read(
    StlAPI_Reader * reader, TopoDS_Shape * shape, char* filename,
    HSExceptionType* exType, void ** exPtr
){
    return hs_handleExWithDefault(
        exType,
        exPtr,
        [reader, shape, filename]{
            return reader->Read(*shape, filename);
        },
        false
    );
} 