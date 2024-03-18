#ifndef HS_RWMESH_CAFREADER_H
#define HS_RWMESH_CAFREADER_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

void hs_RWMesh_CafReader_setDocument(RWMesh_CafReader * reader, Handle(TDocStd_Document) * document);

TopoDS_Shape * hs_RWMesh_CafReader_singleShape(RWMesh_CafReader * reader);

bool hs_RWMesh_CafReader_perform(RWMesh_CafReader * reader, char * filename, Message_ProgressRange * progress);

#ifdef __cplusplus
}
#endif

#endif // HS_RWMESH_CAFREADER_H