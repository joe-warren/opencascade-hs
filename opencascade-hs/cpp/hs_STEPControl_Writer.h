#ifndef HS_STEPCONTROL_WRITER_H
#define HS_STEPCONTROL_WRITER_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

STEPControl_Writer * hs_new_STEPControl_Writer();

void hs_delete_STEPControl_Writer(STEPControl_Writer * writer);

void hs_STEPControl_Writer_setTolerance(STEPControl_Writer * writer, double tolerance);

void hs_STEPControl_Writer_unsetTolerance(STEPControl_Writer * writer);

IFSelect_ReturnStatus hs_STEPControl_Writer_transfer(STEPControl_Writer * writer, TopoDS_Shape * shape, STEPControl_StepModelType mode, bool compgraph);

IFSelect_ReturnStatus hs_STEPControl_Writer_write(STEPControl_Writer* writer, char* filename);

#ifdef __cplusplus
}
#endif

#endif // HS_STEPCONTROL_WRITER_H
