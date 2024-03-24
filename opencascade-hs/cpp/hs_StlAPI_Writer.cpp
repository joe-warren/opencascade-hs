#include <StlAPI_Reader.hxx>
#include "hs_StlAPI_Reader.h"

StlAPI_Reader * hs_new_StlAPI_Reader(){
    return new StlAPI_Reader();
}

void hs_delete_StlAPI_Reader(StlAPI_Reader * reader){
    delete reader;
}

bool hs_StlAPI_Reader_read(StlAPI_Reader * reader, TopoDS_Shape * shape, char* filename){
    return reader->Read(*shape, filename);
} 