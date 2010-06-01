#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <SDL/SDL.h>
#include <limits.h>

/* Copyright related stuff:
   This loosely was based on microsand2 by  Max Nagl at www.siebn.de
   http://siebn.de/burningsand/microsand

   And I mean loosely because all that was taken was the idea of using
   the framebuffer and the SDL calls. Everything else was new. The
   author had a very awkward license saying it is allowed to be used
   in opensource software. So because of that we're going to GPL3 this
   just to be safe.

   I really believe this to be a clean reimplementation but I did see
the original source code.  */
/*
    Abram Hindle - Auto Sand - a simple sandgen
    Copyright (C) 2010 Abram Hindle and Max Nagl

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


/* change this if you add a new type */
#define COFF 0

#include "auto-test.h"

#define WEIGHT(VAL) VAL
#define MAXBUFFER 1024
#define RESW 640
#define RESH 480
#define BPP 8
#define COLOR(VAL) (VAL)
/* , VAL2) ((Uint8)(!VAL==NOTHING?VAL2:VAL)) */
#define BOUNDS( X, Y) ((X >= 0 && X < RESW) && (Y >= 0 && Y < RESH))
#define XYINDEX( X , Y)  (RESW * Y + X)
#define IMPOSSIBLE (-1)

void swap_entities( Entity * arr , int i, int j ) {
  Entity tmp = arr[i];
  arr[i] = arr[j];
  arr[j] = tmp;
}

#define min(a, b) (((a) < (b)) ? (a) : (b))
#define max(a, b) (((a) > (b)) ? (a) : (b))

int randint(int i) {
  int k = rand();
  return k % i;
}

int rand_decision(int i, int j) { /* rand_decision(3,4) is 75% yes */
  return ((rand() % j)  < i);
}

void shuffle( int v[], int size ) {
  for (int i = size; i > 1; i--) {
    int j = randint(i); /* 0 <= j <= i-1 (0-based array) */
    int tmp = v[j];
    v[j] = v[i-1];
    v[i-1] = tmp;
  }
}


int main(int v,char**argv){

  /* SDL_Surface * s = SDL_SetVideoMode(RESW , RESH, BPP, SDL_FULLSCREEN);  */
  SDL_Surface * s = SDL_SetVideoMode(RESW , RESH, BPP, 0); 
  SDL_Event e;
  SDL_MouseMotionEvent m;
  int * schedule = malloc( sizeof(int) * RESW * RESH );
  Uint8 * bmp = (Uint8*) s->pixels;
  Entity * entities = malloc( sizeof(Entity) * RESW * RESH );
  Entity cursor = types[0];
  int x, y, j, i, sched;
  int index;
  Entity entity;
  int brush = 9; /* brush size */
  atexit(SDL_Quit);
  SDL_WM_SetCaption("Auto",0);
  srand((unsigned int) time( NULL ));
  /* make the random schedule */
  /* and init Terrain and Entity */
  for (j = 0 ; j < RESW * RESH ; j++ ) {
    schedule[j] = j;
    entities[j] = NOTHING;
  }
  shuffle(schedule, RESW * RESH);  
  while (!SDL_Flip(s)) {
    /* rand pixel schedule */
    /* there will be garbage at the edges */
    for (sched = 0; sched < (RESW * RESH) ; sched++) { 
      j  = schedule[sched]; 
      entity = entities[j];
      x = j % RESW;
      y = j / RESW;      
      /* LOGIC HERE */

      
      
    } /* per pixel */

    /* now draw the buffer! */
    for (j = 0; j < RESW * RESH; j++) {
      bmp[j] = COLOR( entities[j] );
    }                                       

    while (SDL_PollEvent(&e)) {
      switch (e.type) {
      case SDL_KEYDOWN:
        /* other characters wrap */
        cursor = types[ (COFF + e.key.keysym.sym) % NTYPES ];
        /* exit */
	if (e.key.keysym.sym == 'x') {
		exit(0);               
        } else if (e.key.keysym.sym == '+') {
          /* brush size keys */
          brush = min(300,brush+1);
        } else if (e.key.keysym.sym == '=') {
          brush = min(300,brush+1);
        } else if (e.key.keysym.sym == '-') {
          brush = max(1,brush-1);
        } else if (e.key.keysym.sym == 'c') {
          for (int i = 0 ; i < RESW * RESH; i++ ) {
            entities[i] = NOTHING;
          }
	} 
        break;
      case SDL_QUIT:
        exit(0);
      case SDL_MOUSEMOTION:
      case SDL_MOUSEBUTTONDOWN:
        m = e.motion;
        /* paint in the cursor on click */
        if (m.state) {
          for (y = m.y - brush; y < m.y + brush; y++ ) {
            for (x = m.x - brush; x < m.x + brush; x++ ) {
              if (x >= 0 && x < RESW && y >= 0 && y < RESH) {
                entities[ y * RESW + x ] = cursor;
              }
            }
          } /* for */
        } /* if state */
      } /* event type */
    } /* Poll */
  }
  exit(0);
  return 0;
}

