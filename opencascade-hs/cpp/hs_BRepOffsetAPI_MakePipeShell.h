#ifndef HS_BREPOFFSETAPI_MAKEPIPESHELL_H
#define HS_BREPOFFSETAPI_MAKEPIPESHELL_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepOffsetAPI_MakePipeShell * hs_new_BRepOffsetAPI_MakePipeShell(TopoDS_Wire * spine);

void hs_delete_BRepOffsetAPI_MakePipeShell(BRepOffsetAPI_MakePipeShell * builder);

void hs_BRepOffsetAPI_MakePipeShell_setModeFrenet(BRepOffsetAPI_MakePipeShell * builder, bool isFrenet);

void hs_BRepOffsetAPI_MakePipeShell_setDiscreteMode(BRepOffsetAPI_MakePipeShell * builder);

void hs_BRepOffsetAPI_MakePipeShell_add(BRepOffsetAPI_MakePipeShell * builder, TopoDS_Wire * profile, bool withContact, bool withCorrection);

bool hs_BRepOffsetAPI_MakePipeShell_makeSolid(BRepOffsetAPI_MakePipeShell * builder);

bool hs_BRepOffsetAPI_MakePipeShell_isDone(BRepOffsetAPI_MakePipeShell * builder);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPOFFSETAPI_MAKEPIPESHELL_H
