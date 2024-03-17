#include <XSControl_Reader.hxx>
#include "hs_XSControl_Reader.h"

IFSelect_ReturnStatus hs_XSControl_Reader_readFile(XSControl_Reader * reader, char * filename){
    return reader->ReadFile(filename);
}

TopoDS_Shape * hs_XSControl_Reader_oneShape(XSControl_Reader * reader){
    return new TopoDS_Shape(reader->OneShape());
}