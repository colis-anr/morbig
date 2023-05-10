(
	while read dir; do rmdir "$dir"; done
) << DATA
/usr/local/share/cacti/scripts
DATA

