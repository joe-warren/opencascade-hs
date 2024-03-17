#ifndef HS_XSCONTROL_READER_H
#define HS_XSCONTROL_READER_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

IFSelect_ReturnStatus hs_XSControl_Reader_readFile(XSControl_Reader * reader, char * filename);

bool hs_XSControl_Reader_transferRoots(XSControl_Reader * reader);

TopoDS_Shape * hs_XSControl_Reader_oneShape(XSControl_Reader * reader);

#ifdef __cplusplus
}
#endif

#endif // HS_XSCONTROL_READER_H
