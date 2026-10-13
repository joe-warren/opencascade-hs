#include <BOPAlgo_Tools.hxx>
#include <TopoDS_Shape.hxx>
#include "hs_Exception.h"
#include "hs_BOPAlgo_Tools.h"

TopoDS_Shape * hs_BOPAlgo_Tools_EdgesToWires(TopoDS_Shape * edges, bool shared, double angTol, int * status, HSExceptionType* exType, void ** exPtr){
    return hs_handleEx(exType, exPtr, [edges, shared, angTol, status]{
        TopoDS_Shape * wires = new TopoDS_Shape();
        *status = BOPAlgo_Tools::EdgesToWires(*edges, *wires, shared, angTol);
        return wires;
    });
}

TopoDS_Shape * hs_BOPAlgo_Tools_WiresToFaces(TopoDS_Shape * wires, double angTol, bool * success, HSExceptionType* exType, void ** exPtr){
    return hs_handleEx(exType, exPtr, [wires, angTol, success]{
        TopoDS_Shape * faces = new TopoDS_Shape();
        *success = BOPAlgo_Tools::WiresToFaces(*wires, *faces, angTol);
        return faces;
    });
}
