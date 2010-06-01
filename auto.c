#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <SDL/SDL.h>
#include <Harbinger.h>
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
    Abram Hindle - Clean Sand - a simple sand game
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
#define NTYPES 6
#define COFF 6

typedef enum ENTITY { 
  NOTHING = 0, 
  WALL = 111, 
  SHEEP = 91,
  WOLF = 160,
  SHRUB = 24,
  FIRE = 170,
} Entity;
/*
  6 dark blue
  7 bright blue
  8 dark green
  9 teal
  10 blue
  160 RED
*/

#define EATER(VAL) (VAL == EATERWALL || VAL == ACID)
#define WEIGHT(VAL) VAL
#define FIXED(VAL) (VAL == EATERWALL || VAL == WALL)
#define MAXBUFFER 1024
#define RESW 640
#define RESH 480
#define BPP 8

#define COLOR(VAL, VAL2) ((Uint8)(VAL==NOTHING?VAL2:VAL))
#define BOUNDS( X, Y) ((X >= 0 && X < RESW) && (Y >= 0 && Y < RESH))
#define XYINDEX( X , Y)  (RESW * Y + X)
#define IMPOSSIBLE (-1)
#define FOREACH_DIR( DIR ) for (DIR = UP; DIR <= RIGHT; DIR = next_dir(DIR)) 
#define RAND_EACH_DIR( DIR, DIRS ) direction_combo( DIRS ); for ( int DIRITER = 0; DIRITER < NDIRS; DIR = DIRS[DIRITER++] )

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
  /* 
  if (k >= i * (INT_MAX / i)) {
    return randint(i);
  } else {
    return k % i;
    } */
}


#define DIRCOMBO 24
#define NDIRS 4

typedef enum DIRECTION { NONE = 0, UP = 1, RIGHT = 2, DOWN = 3, LEFT = 4 } Direction;

Direction direction_combos[] = {
  UP, RIGHT,  DOWN, LEFT  ,
  UP, RIGHT,  LEFT, DOWN  ,
  UP, LEFT , RIGHT, DOWN  ,
  UP, LEFT ,  DOWN, RIGHT ,
  UP, DOWN , RIGHT, LEFT  ,
  UP, DOWN ,  LEFT, RIGHT ,
  DOWN, RIGHT,  UP, LEFT  ,
  DOWN, RIGHT,  LEFT, UP  ,
  DOWN, LEFT , RIGHT, UP  ,
  DOWN, LEFT ,  UP, RIGHT ,
  DOWN, UP , RIGHT, LEFT  ,
  DOWN, UP ,  LEFT, RIGHT ,
  RIGHT, DOWN,  UP, LEFT  ,
  RIGHT, DOWN,  LEFT, UP  ,
  RIGHT, LEFT , DOWN, UP  ,
  RIGHT, LEFT ,  UP, DOWN ,
  RIGHT, UP , DOWN, LEFT  ,
  RIGHT, UP ,  LEFT, DOWN ,
  LEFT, DOWN,  UP, RIGHT  ,
  LEFT, DOWN,  RIGHT, UP  ,
  LEFT, RIGHT , DOWN, UP  ,
  LEFT, RIGHT ,  UP, DOWN ,
  LEFT, UP , DOWN, RIGHT  ,
  LEFT, UP ,  RIGHT, DOWN 
};

/* dirs is of size NDIRS */
void direction_combo( Direction dirs[] ) {
  int j = 4 * randint(DIRCOMBO);
  for (int i = 0; i < NDIRS; i++) {
    dirs[i] = direction_combos[j + i];
  }
}


typedef enum TERRAIN {  PLANT = 4, EARTH = 1, ROCK = 3 } Terrain;


Direction next_dir( Direction dir ) {
  switch (dir) {
  case LEFT:
    return UP;
  case NONE: 
    return NONE;
  case UP:
    return RIGHT;
  case RIGHT:
    return DOWN;
  case DOWN:
    return LEFT;
  }
  return NONE;
}
int index_of_direction( int j, int direction ) {
  int x = j % RESW;
  int y = j / RESW;
  switch (direction) {
  case NONE:
    return -1;
    break;
  case UP:
    y = y - 1;     
    break;
  case DOWN:
    y = y + 1;
    break;
  case LEFT:
    x = x - 1;
    break;
  case RIGHT:
    x = x + 1;
    break;
  }
  if (BOUNDS(x,y)) {
    return XYINDEX( x, y);
  } else {
    return IMPOSSIBLE;
  }
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


Direction touch_type(Entity * entities, Entity type, int x, int y, int j) {
  Direction dir = NONE;
  Direction dirs[NDIRS];
  int index;
  RAND_EACH_DIR( dir , dirs) {
    if (IMPOSSIBLE != (index = index_of_direction( j, dir ))) {
      if ( type ==  entities[index]) {
        return dir;
      }
    }
  }
  return NONE;
}
Direction touch_terrain(Terrain * terrians, Entity terrain, int x, int y, int j) {
  Direction dir = NONE;
  Direction dirs[NDIRS];
  int index;
  RAND_EACH_DIR( dir , dirs) {
    if (IMPOSSIBLE != (index = index_of_direction( j, dir ))) {
      if ( terrain ==  terrians[index]) {
        return dir;
      }
    }
  }
  return NONE;
}

Direction touch_wolf( Entity * entities, int x, int y, int j) {
  return touch_type( entities, WOLF, x, y, j);
}
Direction touch_sheep( Entity * entities, int x, int y, int j) {
  return touch_type( entities, SHEEP, x, y, j);
}


Direction touch_nothing( Entity * entities, int x, int y, int j) {
  return (touch_type( entities, NOTHING, x, y, j));
}
Direction touch_shrub( Entity * entities, int x, int y, int j) {
  return (touch_type( entities, SHRUB, x, y, j));
}
Direction touch_terrain_earth( Terrain * terrains, int x, int y, int j) {
  return (touch_terrain( terrains, EARTH, x, y, j));
}
Direction touch_terrain_plant( Terrain * terrains, int x, int y, int j) {
  return (touch_terrain( terrains, PLANT, x, y, j));
}

/* harbinger stuff */
static Harbinger * harb;
static char buffer[MAXBUFFER] = { '\0' };
static char pid[32] = {'\0'};

int consume( Entity * entities, int x, int y, int eater, int eaten) {
  Entity them_entity = entities[eater];
  Entity entity = entities[eaten];
  snprintf( buffer, MAXBUFFER, "Eaten by %d %d %d %d", them_entity, x ,y, entity);
  harbingerSend( harb, buffer );
  entities[eaten] = NOTHING;
  return 1;
}
int death( Entity * entities, int x, int y, int deceased) {
  Entity them_entity = entities[deceased];
  snprintf( buffer, MAXBUFFER, "Death of %d %d %d", them_entity, x ,y);
  harbingerSend( harb, buffer );
  entities[deceased] = NOTHING;
  return 1;
}

int produce( Entity * entities, Entity product, int x, int y, int index) {
  snprintf( buffer, MAXBUFFER, "Spawn of %d %d %d", product, x ,y);
  harbingerSend( harb, buffer );
  entities[index] = product;
  return 1;
}
int produce_terrain( Terrain * terrains, Terrain terrain, int x, int y, int index) {
  snprintf( buffer, MAXBUFFER, "Terrain-Growth of %d %d %d", terrain, x ,y);
  harbingerSend( harb, buffer );
  terrains[index] = terrain;
  return 1;
}





int main(int v,char**argv){
  snprintf(pid,32,"%d",getpid());
  harb = harbingerSingleton("alife", pid, NULL, NULL, 0);
  /* SDL_Surface * s = SDL_SetVideoMode(RESW , RESH, BPP, SDL_FULLSCREEN);  */
  SDL_Surface * s = SDL_SetVideoMode(RESW , RESH, BPP, 0); 
  SDL_Event e;
  SDL_MouseMotionEvent m;
  Entity types[]={NOTHING, SHEEP, WOLF, SHRUB, FIRE, WALL };
  int * schedule = malloc( sizeof(int) * RESW * RESH );
  Uint8 * bmp = (Uint8*) s->pixels;
  Entity * entities = malloc( sizeof(Entity) * RESW * RESH );
  Terrain  * terrains   = malloc( sizeof(Terrain)  * RESW * RESH );
  Entity cursor = NOTHING;
  int x, y, j, i, sched;
  int index;
  Direction direction;
  Entity entity;
  Terrain terrain;
  int brush = 9; /* brush size */
  Terrain cursor_terrain = EARTH; /* the direction specified by the mouse */
  enum MODE { FOREGROUND, BACKGROUND } mode = FOREGROUND;
  atexit(SDL_Quit);
  SDL_WM_SetCaption("Clean Sand",0);
  srand((unsigned int) time( NULL ));
  /* make the random schedule */
  /* and init Terrain and Entity */
  for (j = 0 ; j < RESW * RESH ; j++ ) {
    schedule[j] = j;
    entities[j] = NOTHING;
    terrains[j] = EARTH;
  }
  shuffle(schedule, RESW * RESH);  
  while (!SDL_Flip(s)) {
    /* rand pixel schedule */
    /* there will be garbage at the edges */
    for (sched = 0; sched < RESW * (RESH - 1) ; sched++) { 
      /* for (sched = RESW * RESH - 1; sched >= 0 ; sched--) { */
      j  = schedule[sched]; 
      /* j = sched; */
      terrain  = terrains[j];
      entity = entities[j];
      x = j % RESW;
      y = j / RESW;      
      switch ( entity ) {
      case SHEEP:
        if ( touch_wolf( entities, x, y, j ) ) {
          if ( NONE != ( direction = touch_nothing( entities, x, y, j ) ) ) {
            if ( IMPOSSIBLE != ( index = index_of_direction(j, direction ) ) ) {
              /* run away from wolf */
              swap_entities(entities, j, index); 
            }
          } 
        } else if ((NONE != (direction = touch_shrub( entities, x, y, j ))) && rand_decision(1,4)) {
          /* eat the shrub */
           consume(entities, x,y, j, index_of_direction(j, direction));
           if (rand_decision(3,4) && 
               (NONE != ( touch_sheep( entities, x, y, j ))) &&
               (NONE != (direction = touch_nothing( entities, x, y, j )))) {
             produce(entities, SHEEP, x, y, index_of_direction(j, direction));
           }
           /* } else if (rand_decision(1,50) && 
                   (NONE != ( touch_sheep( entities, x, y, j ))) &&
                   (NONE != (direction = touch_nothing( entities, x, y, j ))) ) { */
          /* mate */
           /* produce(entities, SHEEP, x, y, index_of_direction(j, direction)); */
        } else if ((NONE != (direction = touch_nothing( entities, x, y, j ))) && rand_decision(3,4)) {
          /* move */
          swap_entities(entities, j, index_of_direction(j, direction));
        } else if (rand_decision(1,500)) {
          /* dies */
          death(entities, x,y, j);
        }
        break;
      case WOLF:
        if (NONE != (direction = touch_sheep( entities, x, y, j )) && rand_decision(1,4)) {
          /* eat sheep */
          { 
            int idex = index_of_direction(j, direction);
            consume(entities, x,y, j, idex);
            if (rand_decision(2,4) && (NONE != touch_wolf(entities, x, y, j))) {
              /* have a baby if you ate a sheep */
              produce(entities, WOLF, x, y, idex);
            }
          }
        } else if ((NONE != (direction = touch_nothing( entities, x, y, j ))) && rand_decision(3,4)) {
          /* move to nothing */
          swap_entities( entities, j, index_of_direction( j, direction));
        } else if (rand_decision(1,200)) {
          /* death */
          death(entities, x, y, j);
        }
        break;
      case SHRUB:
        /* spawn plant life */
        if ((NONE != (direction = touch_terrain_earth( terrains, x, y, j ) )) && rand_decision(1,4)) {
          produce_terrain( terrains, PLANT, x,y,index_of_direction( j, direction ));
        } else if (rand_decision(1,100)) {
          /* shrub dies */
          death(entities, x,y, j);
        }
        break;
      case FIRE:
        if ((NONE != (direction = touch_shrub( entities , x, y, j ) )) && rand_decision(2,4)) {
          int idex = index_of_direction(j, direction);
          /* shrub dies */
          consume(entities, x,y, j, idex);
          if (rand_decision(3,4)) {
            /* fire spreads */
            produce(entities, FIRE, x,y, idex);
          }
        } else if ( (NONE != (direction = touch_terrain_plant( terrains, x, y, j ))) && rand_decision(1,4)) {
          int idex = index_of_direction(j, direction);
          if (entities[idex]!=NOTHING) {
            /* whatever dies */
            consume(entities, x,y, j, idex);
          }
          if (rand_decision(3,4)) {
            /* fire spreads */
            produce(entities, FIRE, x,y, idex);
          }
          if (rand_decision(4,5)) {
            /* plants are killed */
            produce_terrain( terrains, EARTH, x,y, idex);
          }
        } else if (rand_decision(1,10)) {
          death( entities, x, y, j);
        }
        break;
      case WALL:
        break;
      case NOTHING:
        if ( terrains[j] == EARTH &&
            (NONE != (direction = touch_terrain_plant( terrains, x, y, j ) )) &&
            rand_decision(1,100)) {
            /* spread plant */
          produce_terrain( terrains, PLANT, x, y, index_of_direction( j, direction ));
        } else if (terrains[j] == PLANT && rand_decision(1,500)) {
          /* grow a shrub */
          produce(entities, SHRUB, x, y, j);
        } 
        break;
      } /* entities dealt with */
      
    } /* per pixel */
    /* top and bottom */
    for ( x = 0; x < RESW; x++ ) {
      entities[x] = NOTHING;
      entities[RESW*(RESH-1) + x] = NOTHING;
    }
    /* left and right */
    for ( y = 0; y < RESH; y++ ) {
      entities[y * RESW + 0] = NOTHING;
      entities[y * RESW + (RESW - 1)] = NOTHING;
    }



    /* now draw the buffer! */
    for (j = 0; j < RESW * RESH; j++) {
      bmp[j] = COLOR( entities[j], terrains[j] );
    }                                       

    while (SDL_PollEvent(&e)) {
      switch (e.type) {
      case 2:
        /* other characters wrap */
        cursor = types[ (COFF + e.key.keysym.sym) % NTYPES ];
        mode = FOREGROUND;
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
            terrains[i] = EARTH;
          }
	} else {
          switch (e.key.keysym.sym) {
          case SDLK_LEFT:
            mode = BACKGROUND;
            cursor_terrain = PLANT;
            break;
          case SDLK_RIGHT:
            mode = BACKGROUND;
            cursor_terrain = EARTH;
            break;
          case SDLK_UP:
            mode = BACKGROUND;
            cursor_terrain = ROCK;
            break;
          case SDLK_DOWN:
            mode = BACKGROUND;
            cursor_terrain = EARTH;
            break;
          default:
            break;
          }
          /* if shift was held then fill the background */
          if (mode == BACKGROUND && (e.key.keysym.mod & (KMOD_LSHIFT | KMOD_RSHIFT))) {
            for (i = 0 ; i < RESW * RESH; i++) {
              terrains[i] = cursor_terrain;
            }
          }
        }
        break;
      case 12:
        exit(0);
      case 4:
      case 5:
        m = e.motion;
        /* paint in the cursor on click */
        if (m.state) {
          if (mode == FOREGROUND) {
            for (y = m.y - brush; y < m.y + brush; y++ ) {
              for (x = m.x - brush; x < m.x + brush; x++ ) {
                if (x >= 0 && x < RESW && y >= 0 && y < RESH) {
                  entities[ y * RESW + x ] = cursor;
                }
              }
            }
          } else if (mode == BACKGROUND) {
            for (y = m.y - brush; y < m.y + brush; y++ ) {
              for (x = m.x - brush; x < m.x + brush; x++ ) {
                if (x >= 0 && x < RESW && y >= 0 && y < RESH) {
                  terrains[ y * RESW + x ] = cursor_terrain;
                }
              }
            }
          }
        } /* check state */
      } /* event type */
    } /* Poll */
  }
  exit(0);
  return 0;
}


    /* test the schedule */
    /* 
    for (j = 0; j < RESW * RESH; j++ ) {
      schedule2[j]=0;
    }
    for (j = 0; j < RESW * RESH; j++ ) {
      schedule2[schedule[j]]++;
    }
    for (i = 0; i < RESW * RESH; i++ ) {
    int c = schedule2[i];
      if (c == 0) {
        printf("C was 0!\n");     
        } else if (c > 1) {
      printf("C occured more than once??\n");
        }
      }
    */
