#ifndef __SDLRUN_H
#define __SDLRUN_H

#include <idris_rts.h>

// Start SDL, open a window with dimensions (x,y)
void* startSDL(int x, int y);

// Drawing primitives
void filledRect(void *s,
	        int x, int y, int w, int h,
	        int r, int g, int b, int a);
void flipBuffers(void* s_in);
void filledEllipse(void* s_in,
		   int x, int y, int rx, int ry,
                   int r, int g, int b, int a);
void drawLine(void* s_in,
	      int x, int y, int ex, int ey,
	      int r, int g, int b, int a);

// Events
void* pollEvent(VM* vm); // builds an Idris value

#endif
