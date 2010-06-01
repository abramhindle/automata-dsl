#define NTYPES 2

typedef enum ENTITY {   NOTHING = 0,  SOMETHING = 222 } Entity;
Entity types[] = {  NOTHING, SOMETHING };
char entity_to_char( Entity e ) { 
switch ( e ) {
    case NOTHING: return '_';

    case SOMETHING: return '@';
 default: return '?'; } }


#define G994_width 3
#define G994_height 3
#define G994_len 9
Entity G994[] = {  NOTHING, NOTHING, NOTHING, NOTHING, SOMETHING, NOTHING, NOTHING, NOTHING, NOTHING };


#define G996_width 3
#define G996_height 3
#define G996_len 9
Entity G996[] = {  NOTHING, SOMETHING, NOTHING, NOTHING, SOMETHING, NOTHING, NOTHING, NOTHING, NOTHING };


#define G997_width 3
#define G997_height 3
#define G997_len 9
Entity G997[] = {  NOTHING, NOTHING, NOTHING, NOTHING, SOMETHING, NOTHING, NOTHING, SOMETHING, NOTHING };


#define G999_width 3
#define G999_height 3
#define G999_len 9
Entity G999[] = {  NOTHING, SOMETHING, NOTHING, NOTHING, SOMETHING, NOTHING, NOTHING, SOMETHING, NOTHING };

#define match_patterns_len 2
Entity * match_patterns[] = {  G994, G997 };
"
