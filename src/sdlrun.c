#include <stdio.h>
#include <SDL/SDL.h>
#include <SDL/SDL_gfxPrimitives.h>

#include <idris_rts.h>

SDL_Surface* graphicsInit(int xsize, int ysize) {
    SDL_Surface *screen;

    if(SDL_Init(SDL_INIT_TIMER | SDL_INIT_VIDEO | SDL_INIT_AUDIO) <0 )
    {
	printf("Unable to init SDL: %s\n", SDL_GetError());
	return NULL;
    }

    screen = SDL_SetVideoMode(xsize, ysize, 32,
                              SDL_HWSURFACE | SDL_DOUBLEBUF);
    if (screen==NULL) {
	printf("Unable to init SDL: %s\n", SDL_GetError());
	return NULL;
    }

    return screen;
}

void filledRect(void *s_in,
	        int x, int y, int w, int h,
	        int r, int g, int b, int a) 
{
    SDL_Surface* s = (SDL_Surface*)s_in;
    Uint32 colour 
	= SDL_MapRGBA(s->format, (Uint8)r, (Uint8)g, (Uint8)b, (Uint8) a);
    SDL_Rect rect = { x, y, w, h };
    SDL_FillRect(s, &rect, colour);
}

void filledEllipse(void* s_in,
		   int x, int y, int rx, int ry,
                   int r, int g, int b, int a) 
{
    SDL_Surface* s = (SDL_Surface*)s_in;
    filledEllipseRGBA(s, x, y, rx, ry, r, g, b, a);
}

void drawLine(void* s_in,
	      int x, int y, int ex, int ey,
	      int r, int g, int b, int a) 
{
    SDL_Surface* s = (SDL_Surface*)s_in;
    lineRGBA(s, x, y, ex, ey, r, g, b, a);
}

void flipBuffers(void* s_in) {
    SDL_Surface* s = (SDL_Surface*)s_in;
    SDL_Flip(s);
}

void* startSDL(int x, int y) {
    SDL_Surface *s = graphicsInit(x, y);
//    drawRect(s, 100, 100, 50, 50, 255, 0, 0, 128);
//    while(1) {
    return (void*)s;
}

/*
data Key = KeyUpArrow
         | KeyDownArrow
	 | KeyLeftArrow
	 | KeyRightArrow
	 | KeyAny Int
*/

VAL KEY(VM* vm, int tag, SDLKey key) {
    VAL k;

    switch(key) {
    case SDLK_UP:
        idris_constructor(k, vm, 0, 0, 0);
	break;
    case SDLK_DOWN:
        idris_constructor(k, vm, 1, 0, 0);
	break;
    case SDLK_LEFT:
        idris_constructor(k, vm, 2, 0, 0);
	break;
    case SDLK_RIGHT:
        idris_constructor(k, vm, 3, 0, 0);
	break;
    default:
        idris_constructor(k, vm, 4, 1, 0);
        // safe because there's no further allocation.
        idris_setConArg(k, 0, MKINT((intptr_t)key));
	break;
    }

    VAL event;
    idris_constructor(event, vm, tag, 1, 0);
    idris_setConArg(event, 0, k);

    return event;
}

/*
data Event = KeyDown Key
           | KeyUp Key
	   | AppQuit

pollEvent : IO (Maybe Event)
*/

void* pollEvent(VM* vm) 
{
    VAL idris_event;

    SDL_Event event; // = (SDL_Event *) GC_MALLOC(sizeof(SDL_Event));
    int r = SDL_PollEvent(&event);

    idris_requireAlloc(vm, 128); // Conservative!

    if (r==0) {
        idris_constructor(idris_event, vm, 0, 0, 0); // Nothing
    }
    else {
	VAL ievent = NULL;
	switch(event.type) {
	case SDL_KEYDOWN:
	    ievent = KEY(vm, 0, event.key.keysym.sym);
	    break;
	case SDL_KEYUP:
	    ievent = KEY(vm, 1, event.key.keysym.sym);
	    break;
	case SDL_QUIT:
	    idris_constructor(ievent, vm, 2, 0, 0);
	    break;
	default:
	    idris_constructor(idris_event, vm, 0, 0, 0); // Nothing
            idris_doneAlloc(vm);
            return idris_event;
	}
        idris_constructor(idris_event, vm, 1, 1, 0);
        idris_setConArg(idris_event, 0, ievent); // Just ievent
    }

    idris_doneAlloc(vm);
    return idris_event;
}

/*
int main(int argc, char* argv[]) {
    SDL_Surface *s = graphicsInit(640,480);
    SDL_Event event;
    filledRect(s, 100, 100, 50, 50, 255, 0, 0, 128);
    flipBuffers(s);
    int done = 0;
    while(!done) {
        int r = SDL_PollEvent(&event);
        if (r != 0) {
            switch(event.type) {
            case SDL_KEYUP:
                done = 1;
                break;
            default:
                break;
            }
        }
    }
}
*/

