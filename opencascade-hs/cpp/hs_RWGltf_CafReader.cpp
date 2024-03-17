#include <RWGltf_CafReader.hxx>
#include "hs_RWGltf_CafReader.h"

RWGltf_CafReader * hs_new_RWGltf_CafReader(void){
    return new RWGltf_CafReader();
}

void hs_delete_RWGltf_CafReader(RWGltf_CafReader *reader){
    delete reader;
}