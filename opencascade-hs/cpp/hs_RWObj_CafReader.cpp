#include <RWObj_CafReader.hxx>
#include "hs_RWObj_CafReader.h"

RWObj_CafReader * hs_new_RWObj_CafReader(void){
    return new RWObj_CafReader();
}

void hs_RWObj_CafReader_setSinglePrecision(RWObj_CafReader *reader, bool isSingle){
    reader->SetSinglePrecision(isSingle);
}

void hs_delete_RWObj_CafReader(RWObj_CafReader *reader){
    delete reader;
}