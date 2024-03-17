#include <STEPControl_Reader.hxx>
#include "hs_STEPControl_Reader.h"

STEPControl_Reader * hs_new_STEPControl_Reader(void){
    return new STEPControl_Reader();
}

void hs_delete_STEPControl_Reader(STEPControl_Reader * reader){
    delete reader;
}
