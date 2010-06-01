
#define NTYPES 2

typedef enum ENTITY {   NOTHING = 0,  SOMETHING = 222 } Entity;
Entity types[] = {  NOTHING, SOMETHING };
char entity_to_char( Entity e ) { 
switch ( e ) {
    case NOTHING: return '_';

    case SOMETHING: return '@';
 default: return '?'; } }


#define G616_width 3
#define G616_height 3
#define G616_len 9
Entity G616[] = {  NOTHING, NOTHING, NOTHING, NOTHING, SOMETHING, NOTHING, NOTHING, NOTHING, NOTHING };


#define G618_width 3
#define G618_height 3
#define G618_len 9
Entity G618[] = {  NOTHING, SOMETHING, NOTHING, NOTHING, SOMETHING, NOTHING, NOTHING, NOTHING, NOTHING };


#define G619_width 3
#define G619_height 3
#define G619_len 9
Entity G619[] = {  NOTHING, NOTHING, NOTHING, NOTHING, SOMETHING, NOTHING, NOTHING, SOMETHING, NOTHING };


#define G621_width 3
#define G621_height 3
#define G621_len 9
Entity G621[] = {  NOTHING, SOMETHING, NOTHING, NOTHING, SOMETHING, NOTHING, NOTHING, SOMETHING, NOTHING };


#define G622_width 3
#define G622_height 3
#define G622_len 9
Entity G622[] = {  NOTHING, SOMETHING, NOTHING, NOTHING, SOMETHING, NOTHING, NOTHING, NOTHING, NOTHING };


#define G624_width 3
#define G624_height 3
#define G624_len 9
Entity G624[] = {  NOTHING, SOMETHING, NOTHING, NOTHING, SOMETHING, NOTHING, SOMETHING, NOTHING, SOMETHING };


#define G625_width 3
#define G625_height 3
#define G625_len 9
Entity G625[] = {  SOMETHING, NOTHING, SOMETHING, NOTHING, NOTHING, NOTHING, NOTHING, NOTHING, NOTHING };


#define G627_width 3
#define G627_height 3
#define G627_len 9
Entity G627[] = {  SOMETHING, NOTHING, SOMETHING, NOTHING, NOTHING, NOTHING, SOMETHING, NOTHING, SOMETHING };

#define match_patterns_len 4
Entity * match_patterns[] = {  G616, G619, G622, G625 };

#define match_patterns_len 4
Entity * replace_patterns[] = {  G618, G621, G624, G627 };
