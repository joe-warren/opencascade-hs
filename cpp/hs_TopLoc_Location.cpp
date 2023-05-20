#include <TopLoc_Location.hxx>
#include "hs_TopLoc_Location.h"

TopLoc_Location * hs_new_TopLoc_Location(){
    return new TopLoc_Location();
}

TopLoc_Location * hs_new_TopLoc_Location_fromGPTrsf(gp_Trsf * trsf){
    return new TopLoc_Location(*trsf);
}

void hs_delete_TopLoc_Location(TopLoc_Location * l){
    delete l;
}

bool hs_TopLoc_Location_IsIdentity(TopLoc_Location * l){
    return l->IsIdentity();
}

int hs_TopLoc_Location_FirstPower(TopLoc_Location * l){
    return l->FirstPower();
}

TopLoc_Location * hs_TopLoc_Location_NextLocation(TopLoc_Location * l){
    return new TopLoc_Location(l->NextLocation());
}

TopLoc_Location * hs_TopLoc_Location_Inverted(TopLoc_Location * l){
    return new TopLoc_Location(l->Inverted());
}

TopLoc_Location * hs_TopLoc_Location_Multiplied(TopLoc_Location * a, TopLoc_Location * b){
    return new TopLoc_Location(a->Multiplied(*b));
}

TopLoc_Location * hs_TopLoc_Location_Divided(TopLoc_Location * a, TopLoc_Location * b){
    return new TopLoc_Location(a->Divided(*b));
}

TopLoc_Location * hs_TopLoc_Location_Predivided(TopLoc_Location * a, TopLoc_Location * b){
    return new TopLoc_Location(a->Predivided(*b));
}

TopLoc_Location * hs_TopLoc_Location_Powered(TopLoc_Location * l, int p){
    return new TopLoc_Location(l->Powered(p));
}

gp_Trsf * hs_TopLoc_Location_toGPTrsf(TopLoc_Location * l){
    return new gp_Trsf(gp_Trsf(*l));
}

bool hs_TopLoc_Location_IsEqual(TopLoc_Location * a, TopLoc_Location * b){
    return a->IsEqual(*b);
}

bool hs_TopLoc_Location_IsDifferent(TopLoc_Location * a, TopLoc_Location * b){
    return a->IsDifferent(*b);
}

void hs_TopLoc_Location_Clear(TopLoc_Location * l){
    l->Clear();
}
