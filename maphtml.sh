#! /bin/bash
# Copyright (C) Eric Willisson 2011
# This library uses the GNU GPL v3.0 or greater
# see http://www.gnu.org/copyleft/gpl.html for details

# This script takes a Graphviz file and writes out the corresponding
# PNG image and creates the HTML page using that image as an
# imagemap.

input=$1
if [ -z $input ]
then
    input=relationgraph.dot
fi

output=$2
if [ -z $output ]
then
    output=relationmap.html
fi

dot -Tcmapx -omap.html -Tpng -o"${input}map.png" $input
insertpoint=`grep -n '<!-- INSERT map.html HERE -->' $RELATION_MAP_HOME/relationgraph.html | awk -F : '{print $1}'`
head -$insertpoint $RELATION_MAP_HOME/relationgraph.html > $output
sed -i "s/\$NAMEmap.png/${input}map.png/" $output
cat map.html >> $output
echo "</body></html>" >> $output
