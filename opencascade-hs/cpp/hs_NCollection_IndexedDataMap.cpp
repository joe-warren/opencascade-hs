#include <NCollection_IndexedDataMap.hxx>
#include <TCollection_AsciiString.hxx>
#include "hs_NCollection_IndexedDataMap.h"

INDEXED_DATA_MAP(TCollection_AsciiString, TCollection_AsciiString) * hs_new_NCollection_IndexedDataMap_TCollection_AsciiString_TCollection_AsciiString(){
    return new NCollection_IndexedDataMap<TCollection_AsciiString, TCollection_AsciiString>();
}

void hs_delete_NCollection_IndexedDataMap_TCollection_AsciiString_TCollection_AsciiString(INDEXED_DATA_MAP(TCollection_AsciiString, TCollection_AsciiString) * theMap){
    delete theMap;
}
