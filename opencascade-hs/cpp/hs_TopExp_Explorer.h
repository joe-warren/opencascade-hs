#ifndef HS_BREPFILLETAPI_MAKEFILLET_H
#define HS_BREPFILLETAPI_MAKEFILLET_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

TopExp_Explorer * hs_new_TopExp_Explorer(TopoDS_Shape * shape, TopAbs_ShapeEnum toFind);

void hs_delete_TopExp_Explorer(TopExp_Explorer * explorer);

bool hs_TopExp_Explorer_more(TopExp_Explorer * explorer);

void hs_TopExp_Explorer_next(TopExp_Explorer * explorer);

TopoDS_Shape * hs_TopExp_Explorer_value(TopExp_Explorer * explorer);
#ifdef __cplusplus
}
#endif

#endif // HS_TOPEXP_EXPLORER_H