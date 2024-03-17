#ifndef HS_RWGLTF_CAFREADER_H
#define HS_RWGLTF_CAFREADER_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

RWGltf_CafReader * hs_new_RWGltf_CafReader(void);

void hs_delete_RWGltf_CafReader(RWGltf_CafReader *reader);

#ifdef __cplusplus
}
#endif

#endif // HS_RWGLTF_CAFREADER_H