#include <Font_BRepFont.hxx>
#include "hs_Font_BRepFont.h"

Font_BRepFont * hs_new_Font_BRepFont_fromStringAndSize(char * path, double size){
    return new Font_BRepFont(path, size);
}

void hs_delete_Font_BRepFont(Font_BRepFont * font){
    delete font;
}

double hs_Font_BRepFont_ascender(Font_BRepFont * font){
    return font->Ascender();
}

double hs_Font_BRepFont_descender(Font_BRepFont * font){
    return font->Descender();
}

double hs_Font_BRepFont_lineSpacing(Font_BRepFont * font){
    return font->LineSpacing();
}

double hs_Font_BRepFont_pointSize(Font_BRepFont * font){
    return font->LineSpacing();
}