#include <XSControl_Reader.hxx>
#include "hs_XSControl_Reader.h"

IFSelect_ReturnStatus hs_XSControl_Reader_readFile(XSControl_Reader * reader, char * filename){
    return reader->ReadFile(filename);
}

bool hs_XSControl_Reader_transferRoots(XSControl_Reader * reader){
    return reader->TransferRoots();
}

TopoDS_Shape * hs_XSControl_Reader_oneShape(XSControl_Reader * reader){
    return new TopoDS_Shape(reader->OneShape());
}