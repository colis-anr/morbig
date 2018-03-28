dump_databases() {							# {{{
	(get_suffix | while read suffix; do
		dbdir=x
	done) 5<&0 </dev/null
}
