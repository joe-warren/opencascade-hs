#ifndef HS_GPROP_GPROPS_H
#define HS_GPROP_GPROPS_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

GProp_GProps * hs_new_GProp_GProps(
    HSExceptionType* exType, void ** exPtr
);

GProp_GProps * hs_new_GProp_GProps_fromSystemLocation(
    gp_Pnt * pnt,
    HSExceptionType* exType, void ** exPtr
);

void  hs_delete_GProp_GProps(GProp_GProps * props);

double hs_GProp_GProps_mass(
    GProp_GProps * props,
    HSExceptionType* exType, void ** exPtr
);

gp_Pnt * hs_GProp_GProps_centreOfMass(
    GProp_GProps * props,
    HSExceptionType* exType, void ** exPtr
);

double hs_GProp_GProps_momentOfInertia(
    GProp_GProps * props, gp_Ax1 * ax,
    HSExceptionType* exType, void ** exPtr
);

#ifdef __cplusplus
}
#endif

#endif // HS_GPROP_GPROPS
