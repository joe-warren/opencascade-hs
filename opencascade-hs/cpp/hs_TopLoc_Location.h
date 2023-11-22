#ifndef HS_TOPLOC_LOCATION_H
#define HS_TOPLOC_LOCATION_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

TopLoc_Location * hs_new_TopLoc_Location();

TopLoc_Location * hs_new_TopLoc_Location_fromGPTrsf(gp_Trsf * trsf);

void hs_delete_TopLoc_Location(TopLoc_Location * l);

bool hs_TopLoc_Location_IsIdentity(TopLoc_Location * l);

int hs_TopLoc_Location_FirstPower(TopLoc_Location * l);

TopLoc_Location * hs_TopLoc_Location_NextLocation(TopLoc_Location * l);

TopLoc_Location * hs_TopLoc_Location_Inverted(TopLoc_Location * l);

TopLoc_Location * hs_TopLoc_Location_Multiplied(TopLoc_Location * a, TopLoc_Location * b);

TopLoc_Location * hs_TopLoc_Location_Divided(TopLoc_Location * a, TopLoc_Location * b);

TopLoc_Location * hs_TopLoc_Location_Predivided(TopLoc_Location * a, TopLoc_Location * b);

TopLoc_Location * hs_TopLoc_Location_Powered(TopLoc_Location * l, int p);

gp_Trsf * hs_TopLoc_Location_toGPTrsf(TopLoc_Location * l);

bool hs_TopLoc_Location_IsEqual(TopLoc_Location * a, TopLoc_Location * b);

bool hs_TopLoc_Location_IsDifferent(TopLoc_Location * a, TopLoc_Location * b);

void hs_TopLoc_Location_Clear(TopLoc_Location * l);

#ifdef __cplusplus
}
#endif

#endif // HS_TOPLOC_LOCATION_H
