case "$1" in
    configure)
	if [ $# -ge 3 ]; then
		invoke-rc.d quota start
	fi
    ;;
esac
