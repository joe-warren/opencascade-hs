#ifndef HS_FONT_BREPFONT_H
#define HS_FONT_BREPFONT_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

Font_BRepFont * hs_new_Font_BRepFont();

Font_BRepFont * hs_new_Font_BRepFont_fromStringAndSize(char * path, double size);

bool hs_Font_BRepFont_initPathAndSize(Font_BRepFont * font, char * path, double size);

bool hs_Font_BRepFont_initNameAspectAndSize(Font_BRepFont * font, char * name, Font_FontAspect aspect, double size);

void hs_delete_Font_BRepFont(Font_BRepFont * font);

double hs_Font_BRepFont_ascender(Font_BRepFont * font);

double hs_Font_BRepFont_descender(Font_BRepFont * font);

double hs_Font_BRepFont_lineSpacing(Font_BRepFont * font);

double hs_Font_BRepFont_pointSize(Font_BRepFont * font);

#ifdef __cplusplus
}
#endif

#endif // HS_FONT_BREPFONT_H
