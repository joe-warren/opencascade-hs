#ifndef HS_STEPCONTROL_READER_H
#define HS_STEPCONTROL_READER_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

STEPControl_Reader * hs_new_STEPControl_Reader(void);

void hs_delete_STEPControl_Reader(STEPControl_Reader * reader);

#ifdef __cplusplus
}
#endif

#endif // HS_STEPCONTROL_READER_H
