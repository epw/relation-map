#! /usr/bin/env python

import cgi, cgitb
cgitb.enable ()
import subprocess, tempfile

form = cgi.FieldStorage ()

map_string = form.getfirst ("map")

p = subprocess.Popen (["/home/eric/bin/write-relation-map",
                       "-u", "nodes", "-d", "defs"], stdin=subprocess.PIPE,
                      stdout=subprocess.PIPE)
dot = p.communicate (map_string)[0]

dotfile = tempfile.NamedTemporaryFile (delete=False)

dotfile.write (dot)

filename = dotfile.name

dotfile.close ()

p = subprocess.Popen (["dot", "-Tpng", "-o%s.png" % filename, filename])

print """Content-type: text/html

<html>
<body>
<div>
<a href="%s">Graphviz file</a>
<a href="%s.png">Image</a>
</div>

<div>
<img src="%s.png" />
</div>
</body>
</html>
""" % (filename, filename, filename)
