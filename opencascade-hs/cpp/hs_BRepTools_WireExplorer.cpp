#include <BRepTools_WireExplorer.hxx>
#include "hs_BRepTools_WireExplorer.h"

BRepTools_WireExplorer * hs_new_BRepTools_WireExplorer_fromWire(TopoDS_Wire *wire){
    return new BRepTools_WireExplorer(*wire);
}

void hs_delete_BRepTools_WireExplorer(BRepTools_WireExplorer * explorer){
    delete explorer;
}

bool hs_BRepTools_WireExplorer_more(BRepTools_WireExplorer * explorer){
    return explorer->More();
}

void hs_BRepTools_WireExplorer_next(BRepTools_WireExplorer * explorer){
    explorer->Next();
}

TopoDS_Edge * hs_BRepTools_WireExplorer_current(BRepTools_WireExplorer * explorer){
    return (TopoDS_Edge *) &(explorer->Current());
}

TopAbs_Orientation hs_BRepTools_WireExplorer_orientation(BRepTools_WireExplorer * explorer){
    return explorer->Orientation();
}