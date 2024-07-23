#include <BRepOffsetAPI_ThruSections.hxx>
#include "hs_BRepOffsetAPI_ThruSections.h"

BRepOffsetAPI_ThruSections * hs_new_BRepOffsetAPI_ThruSections(bool isSolid, bool ruled, double pres3d){
    return new BRepOffsetAPI_ThruSections(isSolid, ruled, pres3d);
}

void hs_delete_BRepOffsetAPI_ThruSections(BRepOffsetAPI_ThruSections* thruSections){
    delete thruSections;
}

void hs_BRepOffsetAPI_ThruSections_addWire(BRepOffsetAPI_ThruSections* thruSections, TopoDS_Wire * wire){
    thruSections->AddWire(*wire);
}

void hs_BRepOffsetAPI_ThruSections_addVertex(BRepOffsetAPI_ThruSections* thruSections, TopoDS_Vertex* vertex){
    thruSections->AddVertex(*vertex);
}