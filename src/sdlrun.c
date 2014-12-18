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

VAL MOTION(VM* vm, int x, int y, int relx, int rely) {
    VAL m;
    idris_constructor(m, vm, 2, 4, 0);
    idris_setConArg(m, 0, MKINT((intptr_t)x));
    idris_setConArg(m, 1, MKINT((intptr_t)y));
    idris_setConArg(m, 2, MKINT((intptr_t)relx));
    idris_setConArg(m, 3, MKINT((intptr_t)rely));
    return m;
}

VAL BUTTON(VM* vm, int tag, int b, int x, int y) {
    VAL button;

    switch(b) {
    case SDL_BUTTON_LEFT:
        idris_constructor(button, vm, 0, 0, 0);
        break;
    case SDL_BUTTON_MIDDLE:
        idris_constructor(button, vm, 1, 0, 0);
        break;
    case SDL_BUTTON_RIGHT:
        idris_constructor(button, vm, 2, 0, 0);
        break;
    case SDL_BUTTON_WHEELUP:
        idris_constructor(button, vm, 3, 0, 0);
        break;
    case SDL_BUTTON_WHEELDOWN:
        idris_constructor(button, vm, 4, 0, 0);
        break;
    default:
        idris_constructor(button, vm, 0, 0, 0);
        break;
    }

    VAL event;
    idris_constructor(event, vm, tag, 3, 0);
    idris_setConArg(event, 0, button);
    idris_setConArg(event, 1, MKINT((intptr_t)x));
    idris_setConArg(event, 2, MKINT((intptr_t)y));

    return event;
}

VAL RESIZE(VM* vm, int w, int h) {
    VAL m;
    idris_constructor(m, vm, 5, 2, 0);
    idris_setConArg(m, 0, MKINT((intptr_t)w));
    idris_setConArg(m, 1, MKINT((intptr_t)h));
    return m;
}
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
    case SDLK_ESCAPE:
        idris_constructor(k, vm, 4, 0, 0);
	break;
    case SDLK_SPACE:
        idris_constructor(k, vm, 5, 0, 0);
	break;
    case SDLK_TAB:
        idris_constructor(k, vm, 6, 0, 0);
	break;
    case SDLK_F1:
        idris_constructor(k, vm, 7, 0, 0);
	break;
    case SDLK_F2:
        idris_constructor(k, vm, 8, 0, 0);
	break;
    case SDLK_F3:
        idris_constructor(k, vm, 9, 0, 0);
	break;
    case SDLK_F4:
        idris_constructor(k, vm, 10, 0, 0);
	break;
    case SDLK_F5:
        idris_constructor(k, vm, 11, 0, 0);
	break;
    case SDLK_F6:
        idris_constructor(k, vm, 12, 0, 0);
	break;
    case SDLK_F7:
        idris_constructor(k, vm, 13, 0, 0);
	break;
    case SDLK_F8:
        idris_constructor(k, vm, 14, 0, 0);
	break;
    case SDLK_F9:
        idris_constructor(k, vm, 15, 0, 0);
	break;
    case SDLK_F10:
        idris_constructor(k, vm, 16, 0, 0);
	break;
    case SDLK_F11:
        idris_constructor(k, vm, 17, 0, 0);
	break;
    case SDLK_F12:
        idris_constructor(k, vm, 18, 0, 0);
	break;
    case SDLK_F13:
        idris_constructor(k, vm, 19, 0, 0);
	break;
    case SDLK_F14:
        idris_constructor(k, vm, 20, 0, 0);
	break;
    case SDLK_F15:
        idris_constructor(k, vm, 21, 0, 0);
	break;
    case SDLK_LSHIFT:
        idris_constructor(k, vm, 22, 0, 0);
	break;
    case SDLK_RSHIFT:
        idris_constructor(k, vm, 23, 0, 0);
	break;
    case SDLK_LCTRL:
        idris_constructor(k, vm, 24, 0, 0);
	break;
    case SDLK_RCTRL:
        idris_constructor(k, vm, 25, 0, 0);
	break;
    default:
        idris_constructor(k, vm, 26, 1, 0);
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
data Button = Left | Middle | Right | WheelUp | WheelDown

data Event = KeyDown Key
           | KeyUp Key
           | MouseMotion Int Int Int Int
           | MouseButtonDown Button Int Int
           | MouseButtonUp Button Int Int
	   | AppQuit

pollEvent : IO (Maybe Event)
*/

void* pollEvent(VM* vm) 
{
    VAL idris_event;

    SDL_Event event; // = (SDL_Event *) GC_MALLOC(sizeof(SDL_Event));
    int r = SDL_PollEvent(&event);

    idris_requireAlloc(128); // Conservative!

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
        case SDL_MOUSEMOTION:
            ievent = MOTION(vm, event.motion.x, event.motion.y,
                                event.motion.xrel, event.motion.yrel);
            break;
        case SDL_MOUSEBUTTONDOWN:
            ievent = BUTTON(vm, 3, event.button.button,
                                event.button.x, event.button.y);
            break;
        case SDL_MOUSEBUTTONUP:
            ievent = BUTTON(vm, 4, event.button.button,
                                event.button.x, event.button.y);
            break;
        case SDL_VIDEORESIZE:
            ievent = RESIZE(vm, event.resize.w, event.resize.h);
            break;
	case SDL_QUIT:
	    idris_constructor(ievent, vm, 6, 0, 0);
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

