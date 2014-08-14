#!/bin/sh

# below from http://git.2f30.org/fs/tree/bin/respawn

usage() {
	echo "usage $(basename $0) [-d delay] cmd arg..." 1>&2
	exit 1
}

delay="0"
case "$1" in
	-d)
		delay="$2"
		shift 2
		;;
	-h)
		usage
		;;
esac

if test -z $1; then
	usage
fi

while :; do
	$*
	sleep $delay
done
