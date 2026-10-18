#include <BOPAlgo_Tools.hxx>
#include <TopoDS_Shape.hxx>
#include "hs_Exception.h"
#include "hs_BOPAlgo_Tools.h"

int hs_BOPAlgo_Tools_EdgesToWires(TopoDS_Shape * edges, TopoDS_Shape * wires, bool shared, double angTol, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(exType, exPtr, [edges, wires, shared, angTol]{
        return BOPAlgo_Tools::EdgesToWires(*edges, *wires, shared, angTol);
    }
    , 0
    );
}

bool hs_BOPAlgo_Tools_WiresToFaces(TopoDS_Shape * wires, TopoDS_Shape * faces, double angTol, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(exType, exPtr, [wires, faces, angTol]{
        return BOPAlgo_Tools::WiresToFaces(*wires, *faces, angTol);
    } 
    , false
    );
}
