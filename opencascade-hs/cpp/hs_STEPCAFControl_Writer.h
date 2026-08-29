#ifndef HS_STEPCAFCONTROL_WRITER_H
#define HS_STEPCAFCONTROL_WRITER_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

STEPCAFControl_Writer * hs_new_STEPCAFControl_Writer();

void hs_delete_STEPCAFControl_Writer(STEPCAFControl_Writer * writer);

void hs_STEPCAFControl_Writer_setColorMode(STEPCAFControl_Writer * writer, bool colorMode);

void hs_STEPCAFControl_Writer_setNameMode(STEPCAFControl_Writer * writer, bool nameMode);

bool hs_STEPCAFControl_Writer_transfer(
    STEPCAFControl_Writer * writer, Handle(TDocStd_Document) * document, STEPControl_StepModelType mode,
    HSExceptionType* exType, void ** exPtr
);

IFSelect_ReturnStatus hs_STEPCAFControl_Writer_write(
    STEPCAFControl_Writer * writer, char * filename,
    HSExceptionType* exType, void ** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_STEPCAFCONTROL_WRITER_H
