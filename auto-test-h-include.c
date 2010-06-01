
Entity entity_at( Entity * entities, int dx, int dy, int x, int y ) {
	if ( BOUNDS( dx + x, dy + y ) ) {
		return	entities[ (dx + x) + ( dy + y ) * RESW ];
	} else {
		return IMPOSSIBLE;
	}
}
