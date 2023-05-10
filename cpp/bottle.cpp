// Copyright (c) 2020 OPEN CASCADE SAS
//
// This file is part of the examples of the Open CASCADE Technology software library.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE

#include <BRep_Tool.hxx>

#include <BRepAlgoAPI_Fuse.hxx>

#include <BRepBuilderAPI_MakeEdge.hxx>
#include <BRepBuilderAPI_MakeFace.hxx>
#include <BRepBuilderAPI_MakeWire.hxx>
#include <BRepBuilderAPI_Transform.hxx>

#include <BRepFilletAPI_MakeFillet.hxx>

#include <BRepLib.hxx>

#include <BRepOffsetAPI_MakeThickSolid.hxx>
#include <BRepOffsetAPI_ThruSections.hxx>

#include <BRepPrimAPI_MakeCylinder.hxx>
#include <BRepPrimAPI_MakePrism.hxx>
#include <BRepMesh_IncrementalMesh.hxx>

#include <GC_MakeArcOfCircle.hxx>
#include <GC_MakeSegment.hxx>

#include <GCE2d_MakeSegment.hxx>

#include <gp.hxx>
#include <gp_Ax1.hxx>
#include <gp_Ax2.hxx>
#include <gp_Ax2d.hxx>
#include <gp_Dir.hxx>
#include <gp_Dir2d.hxx>
#include <gp_Pnt.hxx>
#include <gp_Pnt2d.hxx>
#include <gp_Trsf.hxx>
#include <gp_Vec.hxx>

#include <Geom_CylindricalSurface.hxx>
#include <Geom_Plane.hxx>
#include <Geom_Surface.hxx>
#include <Geom_TrimmedCurve.hxx>

#include <Geom2d_Ellipse.hxx>
#include <Geom2d_TrimmedCurve.hxx>

#include <TopExp_Explorer.hxx>

#include <TopoDS.hxx>
#include <TopoDS_Edge.hxx>
#include <TopoDS_Face.hxx>
#include <TopoDS_Wire.hxx>
#include <TopoDS_Shape.hxx>
#include <TopoDS_Compound.hxx>

#include <TopTools_ListOfShape.hxx>

#include <AIS_Shape.hxx>

#include <AIS_InteractiveObject.hxx>
#include <NCollection_Vector.hxx>
#include <TCollection_AsciiString.hxx>

#include <StlAPI_Writer.hxx>

#include "bottle.h"

gp_Pnt * hs_new_GP_Pnt(double x, double y, double z) {
    return new gp_Pnt(x, y, z);
}


gp_Pnt2d * hs_new_GP_Pnt2d(double x, double y) {
    return new gp_Pnt2d(x, y);
}


gp_Dir2d * hs_new_GP_Dir2d(double x, double y) {
    return new gp_Dir2d(x, y);
}


gp_Vec * hs_new_GP_Vec(double x, double y, double z) {
    return new gp_Vec(x, y, z);
}

Handle(Geom_TrimmedCurve) * hs_GCMakeArcOfCircle(gp_Pnt * p1, gp_Pnt * mid, gp_Pnt * p2){
    return new opencascade::handle<Geom_TrimmedCurve>(GC_MakeArcOfCircle(*p1, *mid, *p2));
}

Handle(Geom_TrimmedCurve) * hs_GC_MakeSegment(gp_Pnt * a, gp_Pnt * b){
    return new opencascade::handle<Geom_TrimmedCurve>(GC_MakeSegment(*a, *b));
}

TopoDS_Edge * hs_BRepBuilderAPI_MakeEdgeTrimmedCurve(Handle(Geom_TrimmedCurve) * curve) {
    return new TopoDS_Edge(BRepBuilderAPI_MakeEdge(*curve));
}

TopoDS_Wire * hs_BRepBuilderAPI_MakeWire_2Edges(TopoDS_Edge * a, TopoDS_Edge * b){
    return new TopoDS_Wire(BRepBuilderAPI_MakeWire(*a, *b));
}

TopoDS_Wire * hs_BRepBuilderAPI_MakeWire_3Edges(TopoDS_Edge * a, TopoDS_Edge * b, TopoDS_Edge * c){
    return new TopoDS_Wire(BRepBuilderAPI_MakeWire(*a, *b, *c));
}

gp_Ax1 * hs_gp_Ax1_OX(){ 
    return new gp_Ax1(gp::OX());
}

gp_Ax1 * hs_gp_Ax1_OY(){ 
    return new gp_Ax1(gp::OY());
}

gp_Ax1 * hs_gp_Ax1_OZ(){ 
    return new gp_Ax1(gp::OZ());
}

gp_Dir * hs_gp_Dir_DX(){ 
    return new gp_Dir(gp::DX());
}

gp_Dir * hs_gp_Dir_DY(){ 
    return new gp_Dir(gp::DY());
}

gp_Dir * hs_gp_Dir_DZ(){ 
    return new gp_Dir(gp::DZ());
}

gp_Trsf * hs_new_Trsf(){
    return new gp_Trsf();
}

void hs_Trsf_SetMirror(gp_Trsf * theTransform, gp_Ax1 *theAxis){
  theTransform->SetMirror(*theAxis);
}

BRepBuilderAPI_MakeWire * hs_new_BRepBuilderAPI_MakeWire(){
    return new BRepBuilderAPI_MakeWire();
}

void hs_BRepBuilderAPI_MakeWire_AddWire(BRepBuilderAPI_MakeWire * theBuilder, TopoDS_Wire * theWire){
    theBuilder->Add(*theWire);
}

TopoDS_Wire * hs_BRepBuilderAPI_MakeWire_Wire(BRepBuilderAPI_MakeWire * theBuilder){
    return new TopoDS_Wire(theBuilder->Wire());
}


TopoDS_Face * hs_BRepBuilderAPI_MakeFace_Wire(TopoDS_Wire * theWire){
    return new TopoDS_Face(BRepBuilderAPI_MakeFace(*theWire));
}


TopoDS_Shape * hs_BRepPrimAPI_MakePrism(TopoDS_Face * theFace, gp_Vec * theVector){
    return new TopoDS_Shape(BRepPrimAPI_MakePrism(*theFace, *theVector));
}

BRepFilletAPI_MakeFillet * hs_new_BRepFilletAPI_MakeFillet(TopoDS_Shape * theBody){
    return new BRepFilletAPI_MakeFillet(*theBody);
}

void hs_BRepFilletAPI_MakeFillet_Add(BRepFilletAPI_MakeFillet * theFillet, double r, TopoDS_Edge * theEdge){
    theFillet->Add(r, *theEdge);
}

TopoDS_Shape * hs_BRepFilletAPI_MakeFillet_Shape(BRepFilletAPI_MakeFillet * theFillet){
    return new TopoDS_Shape(theFillet->Shape());
}

TopAbs_ShapeEnum * hs_new_TopAbs_EDGE(){
    return new TopAbs_ShapeEnum(TopAbs_EDGE);
}

TopAbs_ShapeEnum * hs_new_TopAbs_FACE(){
    return new TopAbs_ShapeEnum(TopAbs_FACE);
}

TopExp_Explorer * hs_new_TopExp_Explorer(TopoDS_Shape * theShape, TopAbs_ShapeEnum * toFind){
    return new TopExp_Explorer(*theShape, *toFind);
}

bool hs_TopExp_Explorer_More(TopExp_Explorer * theExplorer){
    return theExplorer->More();
}

TopoDS_Shape * hs_TopExp_Explorer_Current(TopExp_Explorer * theExplorer){
    return new TopoDS_Shape(theExplorer->Current());
}

void hs_TopExp_Explorer_Next(TopExp_Explorer * theExplorer){
    theExplorer->Next();
}

Handle(Geom_Surface) * hs_BRep_Tool_Surface(TopoDS_Face * aFace){
    return new opencascade::handle<Geom_Surface>(BRep_Tool::Surface(*aFace));
}

bool hs_Geom_Surface_isPlane(Handle(Geom_Surface) * aSurface){
    return ((*aSurface)->DynamicType() == STANDARD_TYPE(Geom_Plane));
}

gp_Pnt * hs_Geom_Plane_Location(Handle(Geom_Plane) * aPlane){
      return new gp_Pnt((*aPlane)->Location());
}

double hs_gp_Pnt_X(gp_Pnt * pnt){
    return pnt->X();
}

double hs_gp_Pnt_Y(gp_Pnt * pnt){
    return pnt->Y();
}

double hs_gp_Pnt_Z(gp_Pnt * pnt){
    return pnt->Z();
}

gp_Ax2 * hs_new_gp_Ax2(gp_Pnt * pnt, gp_Dir * dir){
    return new gp_Ax2(*pnt, *dir);
}


gp_Ax2d * hs_new_gp_Ax2d(gp_Pnt2d * pnt, gp_Dir2d * dir){
    return new gp_Ax2d(*pnt, *dir);
}

BRepPrimAPI_MakeCylinder * hs_new_BRepPrimAPI_MakeCylinder(gp_Ax2 * ax2, double radius, double height){
    return new BRepPrimAPI_MakeCylinder(*ax2, radius, height);
}

TopoDS_Shape * hs_BRepPrimAPI_MakeCylinder_Shape(BRepPrimAPI_MakeCylinder * theBuilder){
    return new TopoDS_Shape(theBuilder->Shape());
}

TopoDS_Shape * hs_BRepAlgoAPI_Fuse(TopoDS_Shape *a, TopoDS_Shape *b){
        return new TopoDS_Shape(BRepAlgoAPI_Fuse(*a, *b));
}

TopTools_ListOfShape * hs_new_TopTools_ListOfShape(){
    return new TopTools_ListOfShape();
}

void hs_TopTools_ListOfShape_Append(TopTools_ListOfShape * theList, TopoDS_Shape* theShape){
    theList->Append(*theShape);
}

BRepOffsetAPI_MakeThickSolid * hs_new_BRepOffsetAPI_MakeThickSolid(){
    return new BRepOffsetAPI_MakeThickSolid();
}

void hs_BRepOffsetAPI_MakeThickSolid_MakeThickSolidByJoin(
            BRepOffsetAPI_MakeThickSolid *theMaker,
            TopoDS_Shape* theShape,
            TopTools_ListOfShape * closingFaces,
            double thickness,
            double tolerance){
    theMaker->MakeThickSolidByJoin(*theShape, *closingFaces, thickness, tolerance);
}

TopoDS_Shape * hs_BRepOffsetAPI_MakeThickSolid_Shape(BRepOffsetAPI_MakeThickSolid * theMaker){
    return new TopoDS_Shape(theMaker->Shape());
}

Handle(Geom_CylindricalSurface) * hs_new_GeomCylindricalSurface(gp_Ax2 * ax2, double r){
    return new opencascade::handle<Geom_CylindricalSurface>(new Geom_CylindricalSurface(*ax2, r));
}

Geom2d_Ellipse * hs_new_Geom2d_Ellipse(gp_Ax2d *ax2, double rMajor, double rMinor){
    return new Geom2d_Ellipse(*ax2, rMajor, rMinor);
}

Handle(Geom2d_TrimmedCurve) * hs_new_Geom2d_Trimmed_Curve_fromEllipse(Geom2d_Ellipse * theEllipse, double r1, double r2){
    return new opencascade::handle<Geom2d_TrimmedCurve>(new Geom2d_TrimmedCurve(theEllipse, r1, r2));
}

gp_Pnt2d * hs_Geom2d_Ellipse_Value(Geom2d_Ellipse * theEllipse, double theta){
    return new gp_Pnt2d(theEllipse->Value(theta));
}

Handle(Geom2d_TrimmedCurve) * hs_GCE2d_MakeSegment(gp_Pnt2d * pnt1, gp_Pnt2d * pnt2){
    return new opencascade::handle<Geom2d_TrimmedCurve>(GCE2d_MakeSegment(*pnt1, *pnt2));
}

TopoDS_Edge * hs_BRepBuilderAPI_MakeEdge_fromArcAndCylinder(Handle(Geom2d_TrimmedCurve) * theArc, Handle(Geom_CylindricalSurface) * theSurface){
    return new TopoDS_Edge(BRepBuilderAPI_MakeEdge(*theArc, *theSurface));
}

void hs_BRepLib_BuildCurves3d(TopoDS_Wire * wire){ 
    BRepLib::BuildCurves3d(*wire);
}

BRepOffsetAPI_ThruSections * hs_new_BRepOffsetAPI_ThruSections(bool isSolid){
  return new BRepOffsetAPI_ThruSections(isSolid);
}

void hs_BRepOffsetAPI_ThruSections_AddWire(BRepOffsetAPI_ThruSections * thruSections, TopoDS_Wire * theWire){
    thruSections->AddWire(*theWire);
}

void hs_BRepOffsetAPI_ThruSections_CheckCompatibility(BRepOffsetAPI_ThruSections * thruSections, bool check){
    thruSections->CheckCompatibility(check);
}

TopoDS_Shape * hs_BRepOffsetAPI_ThruSections_Shape(BRepOffsetAPI_ThruSections * thruSections){
    return new TopoDS_Shape(thruSections->Shape());
}

TopoDS_Compound * hs_new_TopoDS_Compound(){
  return new TopoDS_Compound();
}

BRep_Builder * hs_new_BRepBuilder(){
    return new BRep_Builder();
}


void hs_BRep_Builder_MakeCompound(BRep_Builder * theBuilder, TopoDS_Compound * theCompound){
    theBuilder->MakeCompound(*theCompound);
}

void hs_BRep_Builder_Add(BRep_Builder * theBuilder, TopoDS_Compound * theCompound, TopoDS_Shape * theShape){
    theBuilder->Add(*theCompound, *theShape);
}


TopoDS_Shape * MakeBottle (const double theWidth,
                         const double theHeight,
                         const double theThickness)
{
  // Profile : Define Support Points
  gp_Pnt aPnt1(-theWidth / 2., 0, 0);
  gp_Pnt aPnt2(-theWidth / 2., -theThickness / 4., 0);
  gp_Pnt aPnt3(0, -theThickness / 2., 0);
  gp_Pnt aPnt4(theWidth / 2., -theThickness / 4., 0);
  gp_Pnt aPnt5(theWidth / 2., 0, 0);

  // Profile : Define the Geometry
  Handle(Geom_TrimmedCurve) anArcOfCircle = GC_MakeArcOfCircle(aPnt2, aPnt3, aPnt4);
  Handle(Geom_TrimmedCurve) aSegment1 = GC_MakeSegment(aPnt1, aPnt2);
  Handle(Geom_TrimmedCurve) aSegment2 = GC_MakeSegment(aPnt4, aPnt5);

  // Profile : Define the Topology
  TopoDS_Edge anEdge1 = BRepBuilderAPI_MakeEdge(aSegment1);
  TopoDS_Edge anEdge2 = BRepBuilderAPI_MakeEdge(anArcOfCircle);
  TopoDS_Edge anEdge3 = BRepBuilderAPI_MakeEdge(aSegment2);
  TopoDS_Wire aWire = BRepBuilderAPI_MakeWire(anEdge1, anEdge2, anEdge3);

  // Complete Profile
  gp_Ax1 xAxis = gp::OX();
  gp_Trsf aTrsf;

  aTrsf.SetMirror(xAxis);
  BRepBuilderAPI_Transform aBRepTrsf(aWire, aTrsf);
  TopoDS_Shape aMirroredShape = aBRepTrsf.Shape();
  TopoDS_Wire aMirroredWire = TopoDS::Wire(aMirroredShape);

  BRepBuilderAPI_MakeWire mkWire;
  mkWire.Add(aWire);
  mkWire.Add(aMirroredWire);
  TopoDS_Wire myWireProfile = mkWire.Wire();

  // Body : Prism the Profile
  TopoDS_Face myFaceProfile = BRepBuilderAPI_MakeFace(myWireProfile);
  gp_Vec aPrismVec(0, 0, theHeight);
  TopoDS_Shape myBody = BRepPrimAPI_MakePrism(myFaceProfile, aPrismVec);

  // Body : Apply Fillets
  BRepFilletAPI_MakeFillet mkFillet(myBody);
  TopExp_Explorer anEdgeExplorer(myBody, TopAbs_EDGE);
  while (anEdgeExplorer.More())
  {
    TopoDS_Edge anEdge = TopoDS::Edge(anEdgeExplorer.Current());
    //Add edge to fillet algorithm
    mkFillet.Add(theThickness / 12., anEdge);
    anEdgeExplorer.Next();
  }

  myBody = mkFillet.Shape();

  // Body : Add the Neck
  gp_Pnt neckLocation(0, 0, theHeight);
  gp_Dir neckAxis = gp::DZ();
  gp_Ax2 neckAx2(neckLocation, neckAxis);

  Standard_Real myNeckRadius = theThickness / 4.;
  Standard_Real myNeckHeight = theHeight / 10.;

  BRepPrimAPI_MakeCylinder MKCylinder(neckAx2, myNeckRadius, myNeckHeight);
  TopoDS_Shape myNeck = MKCylinder.Shape();

  myBody = BRepAlgoAPI_Fuse(myBody, myNeck);

  // Body : Create a Hollowed Solid
  TopoDS_Face   faceToRemove;
  Standard_Real zMax = -1;

  for (TopExp_Explorer aFaceExplorer(myBody, TopAbs_FACE); aFaceExplorer.More(); aFaceExplorer.Next())
  {
    TopoDS_Face aFace = TopoDS::Face(aFaceExplorer.Current());
    
    // Check if <aFace> is the top face of the bottle¿s neck
    Handle(Geom_Surface) aSurface = BRep_Tool::Surface(aFace);
    if (aSurface->DynamicType() == STANDARD_TYPE(Geom_Plane))
    {
      Handle(Geom_Plane) aPlane = Handle(Geom_Plane)::DownCast(aSurface);
      gp_Pnt aPnt = aPlane->Location();
      Standard_Real aZ = aPnt.Z();
      if (aZ > zMax)
      {
        zMax = aZ;
        faceToRemove = aFace;
      }
    }
  }

  TopTools_ListOfShape facesToRemove;
  facesToRemove.Append(faceToRemove);
  BRepOffsetAPI_MakeThickSolid aSolidMaker;
  aSolidMaker.MakeThickSolidByJoin(myBody, facesToRemove, -theThickness / 50, 1.e-3);
  myBody = aSolidMaker.Shape();
  // Threading : Create Surfaces
  Handle(Geom_CylindricalSurface) aCyl1 = new Geom_CylindricalSurface(neckAx2, myNeckRadius * 0.99);
  Handle(Geom_CylindricalSurface) aCyl2 = new Geom_CylindricalSurface(neckAx2, myNeckRadius * 1.05);

  // Threading : Define 2D Curves
  gp_Pnt2d aPnt(2. * M_PI, myNeckHeight / 2.);
  gp_Dir2d aDir(2. * M_PI, myNeckHeight / 4.);
  gp_Ax2d anAx2d(aPnt, aDir);

  Standard_Real aMajor = 2. * M_PI;
  Standard_Real aMinor = myNeckHeight / 10;

  Handle(Geom2d_Ellipse) anEllipse1 = new Geom2d_Ellipse(anAx2d, aMajor, aMinor);
  Handle(Geom2d_Ellipse) anEllipse2 = new Geom2d_Ellipse(anAx2d, aMajor, aMinor / 4);
  Handle(Geom2d_TrimmedCurve) anArc1 = new Geom2d_TrimmedCurve(anEllipse1, 0, M_PI);
  Handle(Geom2d_TrimmedCurve) anArc2 = new Geom2d_TrimmedCurve(anEllipse2, 0, M_PI);
  gp_Pnt2d anEllipsePnt1 = anEllipse1->Value(0);
  gp_Pnt2d anEllipsePnt2 = anEllipse1->Value(M_PI);

  Handle(Geom2d_TrimmedCurve) aSegment = GCE2d_MakeSegment(anEllipsePnt1, anEllipsePnt2);
  // Threading : Build Edges and Wires
  TopoDS_Edge anEdge1OnSurf1 = BRepBuilderAPI_MakeEdge(anArc1, aCyl1);
  TopoDS_Edge anEdge2OnSurf1 = BRepBuilderAPI_MakeEdge(aSegment, aCyl1);
  TopoDS_Edge anEdge1OnSurf2 = BRepBuilderAPI_MakeEdge(anArc2, aCyl2);
  TopoDS_Edge anEdge2OnSurf2 = BRepBuilderAPI_MakeEdge(aSegment, aCyl2);
  TopoDS_Wire threadingWire1 = BRepBuilderAPI_MakeWire(anEdge1OnSurf1, anEdge2OnSurf1);
  TopoDS_Wire threadingWire2 = BRepBuilderAPI_MakeWire(anEdge1OnSurf2, anEdge2OnSurf2);
  BRepLib::BuildCurves3d(threadingWire1);
  BRepLib::BuildCurves3d(threadingWire2);

  // Create Threading
  BRepOffsetAPI_ThruSections aTool(Standard_True);
  aTool.AddWire(threadingWire1);
  aTool.AddWire(threadingWire2);
  aTool.CheckCompatibility(Standard_False);

  TopoDS_Shape myThreading = aTool.Shape();

  // Building the Resulting Compound
  auto aRes = new TopoDS_Compound();
  BRep_Builder aBuilder;
  aBuilder.MakeCompound(*aRes);
  aBuilder.Add(*aRes, myBody);
  aBuilder.Add(*aRes, myThreading);

  return aRes;
}

int SaveShapeSTL(double resolution, TopoDS_Shape* shape, char* filename){

    //NCollection_Vector<Handle(AIS_InteractiveObject)> object;
    BRepMesh_IncrementalMesh mesh(*shape, 0.01);
    mesh.Perform();
    StlAPI_Writer aStlWriter; 
    if (aStlWriter.Write(*shape, filename))
    {
      std::cout << "A STL file was successfully written" << std::endl; 
    }
    else
    {
      std::cout << "A STL file was not written" << std::endl;
    }
    return 0;
}

