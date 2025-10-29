#ifndef HS_BOPALGO_BOP_H
#define HS_BOPALGO_BOP_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BOPAlgo_BOP * hs_new_BOPAlgo_BOP();

void hs_delete_BOPAlgo_BOP(BOPAlgo_BOP * bop);

void hs_BOPAlgo_BOP_AddTool(BOPAlgo_BOP *bop, TopoDS_Shape * tool);

void hs_BOPAlgo_BOP_SetOperation(BOPAlgo_BOP *bop, BOPAlgo_Operation operation);

#ifdef __cplusplus
}
#endif

#endif // HS_BOPALGO_BOP_H
