(defpackage :plotmap
  (:use :cl :eric)
  (:export :save
	   :graph
	   :write-descs
	   :defplot))

(in-package :plotmap)

(defvar *plots* ())
(defvar *free-nodes* ())
(defvar *edges* ())

(defclass plot ()
  ((name :initarg :name
	 :accessor plot-name)
   (simple-name :initarg :simple-name
		:accessor plot-simple-name)
   (nodes :initarg :nodes
	  :initform nil
	  :accessor plot-nodes)))

(defclass node ()
  ((type :initarg :type
	 :accessor node-type)
   (simple-name :initarg :simple-name
		:accessor node-simple-name)
   (name :initarg :name
	  :accessor node-name)
   (shape :initarg :shape
	  :accessor node-shape)))

(defclass actor (node)
  ((type :initform :actor)
   (shape :initarg :shape
	  :initform 'box)))
(defclass group (actor)
  ((type :initform :group)
   (shape :initarg :shape
	  :initform 'octagon)))
(defclass objective (node)
  ((type :initform :objective)
   (shape :initarg :shape
	  :initform 'ellipse)))
(defclass item (objective)
  ((type :initform :item)
   (shape :initarg :shape
	  :initform 'diamond)))

(defclass edge ()
  ((node1 :initarg :node1
	  :accessor edge-node1)
   (node2 :initarg :node2
	  :accessor edge-node2)
   (type :initarg :type
	 :accessor edge-type)
   (color :initarg :color
	  :accessor edge-color)
   (style :initarg :style
	  :initform 'solid
	  :accessor edge-style)
   (other-attrs :initarg :other-attrs
		:initform '()
		:accessor edge-other-attrs)))

(defun make-plot (name)
  (first (push (make-instance 'plot :name name) *plots*)))

(defun make-node (type name plot)
  (let ((node (make-instance (read-from-string (format nil "~a" type))
			     :name name)))
    (if plot
	(first (push node (plot-nodes plot)))
	(first (push node *free-nodes*)))))

(defun simplify-name (name)
  (strsub (strsub name " " "-") "'" ""))

(defun safe-name (node)
  (if (not (slot-boundp node 'simple-name))
      (simplify-name (node-name node))
      (format nil "~(~a~)" (node-simple-name node))))

(defun node-string (node)
  (format nil "\"~(~a~)\" [label=\"~a\", shape=~(~a~), URL=\"~a.html\"]"
	  (node-simple-name node) (node-name node) (node-shape node)
	  (safe-name node)))

(defun plot-string (plot)
  (format nil "    subgraph cluster_~a {~%        label = \"~a\";~%        color=gray;~%~{        ~a;~%~}    }"
	  (simplify-name (strsub (plot-simple-name plot) "-" "_"))
	  (plot-name plot)
	  (reverse (mapcar #'node-string (plot-nodes plot)))))

(defun edge-string (edge)
  (format nil "\"~(~a~)\" -- \"~(~a~)\" [color=~(~s~), style=~(~a~)~{~^, ~(~a~)=~(~s~)~}];"
	  (node-simple-name (edge-node1 edge))
	  (node-simple-name (edge-node2 edge))
	  (edge-color edge) (edge-style edge)
	  (edge-other-attrs edge)))

(defmacro defrule (rule (&rest args) &body body)
  `(defun ,rule ,args
     (push (make-instance 'edge ,@(first body)
			  :node1 ,(first args) :node2 ,(second args)
			  :type ,(eric:make-keyword rule))
	   *edges*)))

(defrule allies (actor1 actor2)
  (:color 'green))

(defrule allies-weak (actor1 actor2)
  (:color 'green :style 'dashed))

(defrule member-of (actor group)
  (:color 'green :style 'bold))

(defrule enemies (actor1 actor2)
  (:color 'red))

(defrule enemy-oneway (actor1 actor2)
  (:color 'red :other-attrs '(:dir forward)))

(defrule watching (actor1 actor2)
  (:color 'black :style 'dashed :other-attrs '(:dir forward)))

(defrule seeking (actor objective)
  (:color 'blue :other-attrs '(dir forward)))

(defrule possesses (actor node)
  (:color 'purple))

(defrule owns (actor item)
  (:color 'black :other-attrs '(dir forward)))

(defun graph (&key (plots *plots*) (stream *standard-output*) (edges *edges*))
  (format stream "graph G {~%    rankdir = \"LR\"~%~{    ~a~%~}~{~a~%~}~%~{    ~a~%~}};~%"
	  (mapcar #'node-string *free-nodes*)
	  (mapcar #'plot-string plots)
	  (reverse (mapcar #'edge-string edges))))

(defun save (graph)
  (fopen (f "plotgraph.dot" :w)
    (format f "~a" graph)))

(defun write-descs ()
  (let (nodes)
    (dolist (plot *plots*)
      (setf nodes (append nodes (plot-nodes plot))))
    (setf nodes (append nodes *free-nodes*))
    (dolist (node nodes)
      (with-open-file (f (format nil "~a.html" (safe-name node))
			 :direction :output :if-exists nil
			 :if-does-not-exist :create)
	(when f
	  (format f "<html>~%<head>~%<title>~a</title>~%" (node-name node))
	  (format f "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />~%")
	  (format f "<style type=\"text/css\">~%h1 { text-align: center }~%")
	  (format f "</style>~%<body>~%<h1>~a</h1>~%~%</body>~%</html>~%"
		  (node-name node)))))))

(defmacro defnode (var-name type name plot)
  `(progn
     (defparameter ,var-name (make-node ,type ,name ,plot))
     (setf (node-simple-name ,var-name) ',var-name)))

(defmacro defplot (var-name name)
  `(progn
     (if (boundp ',var-name) (makunbound ',var-name))
     (defvar ,var-name (make-instance 'plot :name ,name :nodes ()
				      :simple-name (format nil "~(~a~)"
							   ',var-name)))
     (push ,var-name *plots*)
     ,var-name))

(defun example ()
  (setf *plots* ())
  (setf *edges* ())

  (let (a-terrible-tempest illapa thorana thorasmos cultists new-storm-spirit
			   storm-god war bbeg glamour)

    (defplot a-terrible-tempest "A Terrible Tempest")

    (defnode illapa :actor "Illapa" a-terrible-tempest)
    (defnode thorana :actor "Thorana" a-terrible-tempest)
    (defnode thorasmos :actor "Thorasmos" a-terrible-tempest)
    (defnode cultists :actor "Cultists" a-terrible-tempest)
    (defnode new-storm-spirit :actor "New Storm Spirit" a-terrible-tempest)
    (defnode storm-god :objective "Position of Storm God" a-terrible-tempest)


    (defplot war "War is Brewing")

    (defnode bbeg :actor "BBEG" war)
    (defnode glamour :objective "Glamour" war)
    
    (allies illapa thorana)
    (enemies illapa thorasmos)
    (enemies thorana thorasmos)
    (allies cultists thorasmos)
    (watching cultists thorana)
    
    (seeking illapa storm-god)
    (seeking new-storm-spirit storm-god)
    
    (possesses bbeg glamour)
    (allies bbeg thorasmos)

    (save (graph :stream nil))))
