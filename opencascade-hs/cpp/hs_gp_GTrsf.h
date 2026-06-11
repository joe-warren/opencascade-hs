#ifndef HS_GP_GTRSF_H
#define HS_GP_GTRSF_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif


gp_GTrsf * hs_new_gp_GTrsf();

void hs_delete_gp_GTrsf(gp_GTrsf * t);

void hs_gp_GTrsf_setValue(gp_GTrsf * trsf, int row, int col, double value, HSExceptionType* exType, void ** exPtr);

void hs_gp_GTrsf_setForm(gp_GTrsf * trsf, HSExceptionType* exType, void ** exPtr);

#ifdef __cplusplus
}
#endif

#endif // HS_GP_GTRSF_H
