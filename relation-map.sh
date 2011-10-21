#! /bin/bash
# Copyright (C) Eric Willisson 2011
# This library uses the GNU GPL v3.0 or greater
# see http://www.gnu.org/copyleft/gpl.html for details

# This is the actual program. It passes all of its arguments to
# write-dot-file.rkt, and then calls maphtml.sh on the resulting
# Graphviz file. It then examines the Graphviz file and writes out the
# files linked to by the imagemap for the graph's nodes. It does not
# write over existing HTML files, but creates ones that do not yet
# exist.

# The program abides by the Unix philosophy, and can take the main
# file as the final argument or from stdin. It uses "relation-map" as
# the default filename.

# The README file included with the distribution explains the
# domain-specific language used to make relationship maps.

function last () {
    in_switch=2
    arg=
    for i in $@
    do
	if [ `echo $i | grep '^-'` ]
	then
	    in_switch=1
	else
	    if [ $in_switch = 1 ]
	    then
		in_switch=2
	    else
		in_switch=0
	    fi
	fi
	arg=$i
    done
    if [ $in_switch != 0 ]
    then
	echo "relation-map"
    else
	echo $arg
    fi
}

file=`last $@`

write-dot-file.rkt $* > $file.dot

maphtml.sh $file.dot $file.html

function get_line () {
    head -$2 $1.dot | tail -1
}

for linenum in `grep -n '^\s*"[^"]*" \[' $file.dot | awk -F : '{print $1}'`
do
    line=`get_line $file $linenum`
    name=`echo $line | grep -o '"[^"]*"' | head -1 | sed 's/"//g'`
    url=`echo $line | grep -o '"[^"]*"' | tail -1 | sed 's/"//g'`
    if [ ! -f $url ]
    then
	cp blank.html $url
	sed -i "s/\$TITLE/$name/g" $url
    fi
done
