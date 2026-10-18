#ifndef HS_QUANTITY_COLORRGBA_H
#define HS_QUANTITY_COLORRGBA_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

Quantity_ColorRGBA * hs_new_Quantity_ColorRGBA(Quantity_Color * color, double alpha);

void hs_delete_Quantity_ColorRGBA(Quantity_ColorRGBA * color);

#ifdef __cplusplus
}
#endif

#endif // HS_QUANTITY_COLORRGBA_H
