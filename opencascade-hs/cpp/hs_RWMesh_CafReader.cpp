#include <RWMesh_CafReader.hxx>
#include "hs_RWMesh_CafReader.h"

void hs_RWMesh_CafReader_setDocument(RWMesh_CafReader * reader, Handle(TDocStd_Document) * document){
    reader->SetDocument(*document);
}

TopoDS_Shape * hs_RWMesh_CafReader_singleShape(RWMesh_CafReader * reader){
    return new TopoDS_Shape(reader->SingleShape());
}

bool hs_RWMesh_CafReader_perform(RWMesh_CafReader * reader, char * filename, Message_ProgressRange * progress){
    return reader->Perform(filename, *progress);
}