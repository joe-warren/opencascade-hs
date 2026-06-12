/* Stub emscripten/html5.h for building OCCT with wasi-sdk.
 * Provides minimal type/function declarations that OCCT's Wasm_Window needs.
 */
#ifndef _WASI_STUB_EMSCRIPTEN_HTML5_H
#define _WASI_STUB_EMSCRIPTEN_HTML5_H

#define EMSCRIPTEN_RESULT int
#define EMSCRIPTEN_RESULT_SUCCESS 0
#define EMSCRIPTEN_RESULT_NOT_SUPPORTED -1
#define EMSCRIPTEN_EVENT_TARGET_WINDOW ((const char*)0)
#define EMSCRIPTEN_EVENT_TARGET_DOCUMENT ((const char*)1)

/* Event type constants */
#define EMSCRIPTEN_EVENT_KEYPRESS      1
#define EMSCRIPTEN_EVENT_KEYDOWN       2
#define EMSCRIPTEN_EVENT_KEYUP         3
#define EMSCRIPTEN_EVENT_CLICK         4
#define EMSCRIPTEN_EVENT_MOUSEDOWN     5
#define EMSCRIPTEN_EVENT_MOUSEUP       6
#define EMSCRIPTEN_EVENT_DBLCLICK      7
#define EMSCRIPTEN_EVENT_MOUSEMOVE     8
#define EMSCRIPTEN_EVENT_WHEEL         9
#define EMSCRIPTEN_EVENT_RESIZE        10
#define EMSCRIPTEN_EVENT_SCROLL        11
#define EMSCRIPTEN_EVENT_BLUR          12
#define EMSCRIPTEN_EVENT_FOCUS         13
#define EMSCRIPTEN_EVENT_FOCUSIN       14
#define EMSCRIPTEN_EVENT_FOCUSOUT      15
#define EMSCRIPTEN_EVENT_TOUCHSTART    22
#define EMSCRIPTEN_EVENT_TOUCHEND      23
#define EMSCRIPTEN_EVENT_TOUCHMOVE     24
#define EMSCRIPTEN_EVENT_TOUCHCANCEL   25
#define EMSCRIPTEN_EVENT_MOUSEENTER     20
#define EMSCRIPTEN_EVENT_MOUSELEAVE     21
#define EMSCRIPTEN_EVENT_FULLSCREENCHANGE 30
#define EMSCRIPTEN_EVENT_CANVASRESIZED  37

#define EM_BOOL int
#define EM_TRUE 1
#define EM_FALSE 0
#define EM_UTF8 char

/* DOM delta mode constants */
#define DOM_DELTA_PIXEL 0
#define DOM_DELTA_LINE  1
#define DOM_DELTA_PAGE  2

typedef struct EmscriptenMouseEvent {
  long screenX, screenY, clientX, clientY;
  long targetX, targetY, movementX, movementY;
  int button, buttons;
  EM_BOOL ctrlKey, shiftKey, altKey, metaKey;
  long canvasX, canvasY, padding;
} EmscriptenMouseEvent;

typedef struct EmscriptenWheelEvent {
  EmscriptenMouseEvent mouse;
  double deltaX, deltaY, deltaZ;
  unsigned long deltaMode;
} EmscriptenWheelEvent;

typedef struct EmscriptenKeyboardEvent {
  EM_UTF8 key[32];
  EM_UTF8 code[32];
  unsigned long location;
  EM_BOOL ctrlKey, shiftKey, altKey, metaKey;
  EM_BOOL repeat;
  unsigned long keyCode;
  unsigned long which;
  EM_UTF8 charValue;
  unsigned long charCode;
} EmscriptenKeyboardEvent;

typedef struct EmscriptenTouchPoint {
  long identifier, screenX, screenY, clientX, clientY, pageX, pageY;
  long targetX, targetY, canvasX, canvasY;
  EM_BOOL isChanged;
} EmscriptenTouchPoint;

typedef struct EmscriptenTouchEvent {
  int numTouches;
  EM_BOOL ctrlKey, shiftKey, altKey, metaKey;
  EmscriptenTouchPoint touches[32];
} EmscriptenTouchEvent;

typedef struct EmscriptenUiEvent {
  int documentBodyClientWidth, documentBodyClientHeight;
  int windowInnerWidth, windowInnerHeight;
  int windowOuterWidth, windowOuterHeight;
  int scrollTop, scrollLeft;
} EmscriptenUiEvent;

typedef struct EmscriptenFocusEvent {
  EM_UTF8 nodeName[128];
  EM_UTF8 id[128];
} EmscriptenFocusEvent;

typedef struct EmscriptenFullscreenChangeEvent {
  EM_BOOL isFullscreen;
  int elementWidth, elementHeight, screenWidth, screenHeight;
} EmscriptenFullscreenChangeEvent;

typedef EM_BOOL (*em_mouse_callback_func)(int, const EmscriptenMouseEvent*, void*);
typedef EM_BOOL (*em_wheel_callback_func)(int, const EmscriptenWheelEvent*, void*);
typedef EM_BOOL (*em_key_callback_func)(int, const EmscriptenKeyboardEvent*, void*);
typedef EM_BOOL (*em_touch_callback_func)(int, const EmscriptenTouchEvent*, void*);
typedef EM_BOOL (*em_ui_callback_func)(int, const EmscriptenUiEvent*, void*);
typedef EM_BOOL (*em_focus_callback_func)(int, const EmscriptenFocusEvent*, void*);
typedef EM_BOOL (*em_fullscreenchange_callback_func)(int, const EmscriptenFullscreenChangeEvent*, void*);

/* All registration functions are no-ops */
static inline EMSCRIPTEN_RESULT emscripten_set_mousedown_callback_on_thread(const char* t, void* u, EM_BOOL c, em_mouse_callback_func f, void* p) { (void)t;(void)u;(void)c;(void)f;(void)p; return 0; }
static inline EMSCRIPTEN_RESULT emscripten_set_mouseup_callback_on_thread(const char* t, void* u, EM_BOOL c, em_mouse_callback_func f, void* p) { (void)t;(void)u;(void)c;(void)f;(void)p; return 0; }
static inline EMSCRIPTEN_RESULT emscripten_set_mousemove_callback_on_thread(const char* t, void* u, EM_BOOL c, em_mouse_callback_func f, void* p) { (void)t;(void)u;(void)c;(void)f;(void)p; return 0; }
static inline EMSCRIPTEN_RESULT emscripten_set_wheel_callback_on_thread(const char* t, void* u, EM_BOOL c, em_wheel_callback_func f, void* p) { (void)t;(void)u;(void)c;(void)f;(void)p; return 0; }
static inline EMSCRIPTEN_RESULT emscripten_set_keydown_callback_on_thread(const char* t, void* u, EM_BOOL c, em_key_callback_func f, void* p) { (void)t;(void)u;(void)c;(void)f;(void)p; return 0; }
static inline EMSCRIPTEN_RESULT emscripten_set_keyup_callback_on_thread(const char* t, void* u, EM_BOOL c, em_key_callback_func f, void* p) { (void)t;(void)u;(void)c;(void)f;(void)p; return 0; }
static inline EMSCRIPTEN_RESULT emscripten_set_touchstart_callback_on_thread(const char* t, void* u, EM_BOOL c, em_touch_callback_func f, void* p) { (void)t;(void)u;(void)c;(void)f;(void)p; return 0; }
static inline EMSCRIPTEN_RESULT emscripten_set_touchmove_callback_on_thread(const char* t, void* u, EM_BOOL c, em_touch_callback_func f, void* p) { (void)t;(void)u;(void)c;(void)f;(void)p; return 0; }
static inline EMSCRIPTEN_RESULT emscripten_set_touchend_callback_on_thread(const char* t, void* u, EM_BOOL c, em_touch_callback_func f, void* p) { (void)t;(void)u;(void)c;(void)f;(void)p; return 0; }
static inline EMSCRIPTEN_RESULT emscripten_set_touchcancel_callback_on_thread(const char* t, void* u, EM_BOOL c, em_touch_callback_func f, void* p) { (void)t;(void)u;(void)c;(void)f;(void)p; return 0; }
static inline EMSCRIPTEN_RESULT emscripten_set_resize_callback_on_thread(const char* t, void* u, EM_BOOL c, em_ui_callback_func f, void* p) { (void)t;(void)u;(void)c;(void)f;(void)p; return 0; }
static inline EMSCRIPTEN_RESULT emscripten_set_focus_callback_on_thread(const char* t, void* u, EM_BOOL c, em_focus_callback_func f, void* p) { (void)t;(void)u;(void)c;(void)f;(void)p; return 0; }
static inline EMSCRIPTEN_RESULT emscripten_set_fullscreenchange_callback_on_thread(const char* t, void* u, EM_BOOL c, em_fullscreenchange_callback_func f, void* p) { (void)t;(void)u;(void)c;(void)f;(void)p; return 0; }
static inline EMSCRIPTEN_RESULT emscripten_get_element_css_size(const char* t, double* w, double* h) { (void)t; if(w) *w=0; if(h) *h=0; return 0; }
static inline EMSCRIPTEN_RESULT emscripten_set_element_css_size(const char* t, double w, double h) { (void)t; (void)w; (void)h; return 0; }
static inline EMSCRIPTEN_RESULT emscripten_get_canvas_element_size(const char* t, int* w, int* h) { (void)t; if(w) *w=0; if(h) *h=0; return 0; }
static inline EMSCRIPTEN_RESULT emscripten_set_canvas_element_size(const char* t, int w, int h) { (void)t; (void)w; (void)h; return 0; }
static inline double emscripten_get_device_pixel_ratio(void) { return 1.0; }
static inline EMSCRIPTEN_RESULT emscripten_request_fullscreen(const char* t, EM_BOOL defer) { (void)t; (void)defer; return 0; }
static inline EMSCRIPTEN_RESULT emscripten_exit_fullscreen(void) { return 0; }
static inline EMSCRIPTEN_RESULT emscripten_request_pointerlock(const char* t, EM_BOOL defer) { (void)t; (void)defer; return 0; }
static inline EMSCRIPTEN_RESULT emscripten_exit_pointerlock(void) { return 0; }
static inline void emscripten_sleep(unsigned int ms) { (void)ms; }
static inline void emscripten_async_wget(const char* url, const char* file, void (*onload)(const char*), void (*onerror)(const char*)) { (void)url; (void)file; (void)onload; (void)onerror; }

#endif
