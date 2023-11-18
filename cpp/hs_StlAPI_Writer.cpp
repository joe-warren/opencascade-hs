#include <StlAPI_Writer.hxx>
#include "hs_StlAPI_Writer.h"

StlAPI_Writer * hs_new_StlAPI_Writer(){
    return new StlAPI_Writer();
}

void hs_delete_StlAPI_Writer(StlAPI_Writer * writer){
    delete writer;
}

void hs_StlAPI_Writer_setAsciiMode(StlAPI_Writer * writer, bool asciiMode){
    writer->ASCIIMode() = asciiMode;
}

bool hs_StlAPI_Writer_write(StlAPI_Writer * writer, TopoDS_Shape * shape, char* filename){
    return writer->Write(*shape, filename);
} 