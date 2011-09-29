(load "/home/eric/.sbclrc")

(load "plotmap.lisp")

(defun main ()
  (in-package :plotmap)
  (let ((success nil))
    (handler-case
	(progn
	  (load "plots.lisp")
	  (setf success t))
      (unbound-variable (e) (format t "Error: ~a~%" e))
      (sb-int:simple-program-error (e) (format t "Error: ~a~%" e))
      (sb-kernel::arg-count-error () (format t "Error in defnode, defplot, or defrule~%"))
      (sb-c::input-error-in-compile-file () (format t "Synatx error (are all parentheses balanced?)~%")))
    (when success
      (plotmap:save (plotmap:graph :stream nil))
      (plotmap:write-descs)
      (sb-ext:run-program "./maphtml.sh" ())))
  0)
