#! /bin/bash

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

dot -Tcmapx -omap.html -Tpng -omap.png $input
insertpoint=`grep -n '<!-- INSERT map.html HERE -->' relationgraph.html | awk -F : '{print $1}'`
head -$insertpoint relationgraph.html > $output
cat map.html >> $output
maplength=`wc -l map.html | awk '{print $1}'`
echo "</body></html>" >> $output
