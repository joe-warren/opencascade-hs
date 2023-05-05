#ifndef BOTTLE_H
#define BOTTLE_H

#ifdef __cplusplus
extern "C" {
#else
typedef void TopoDS_Shape;
#endif

TopoDS_Shape * MakeBottle (const double theWidth,
                           const double theHeight,
                           const double theThickness);

int SaveShapeSTL(double res, TopoDS_Shape *shape, char* filename);

#ifdef __cplusplus
}
#endif
#endif /* BOTTLE_H */
