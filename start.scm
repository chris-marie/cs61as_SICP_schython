

(Load "obj.scm")
(load "parser.scm")
(load "py-primitives.scm")
(load "py-meta.scm")
(initialize-python)

;;if you're using emacs, comment out the above code and make the following changes:
;; 1. Locate the directory where your schython files are
;; 2. Prepend the path to all the load commands above
;;
;; for example: if all my files are in a path called:
;;   /path/to/schython
;; my start.scm files would look like:
;;

#|
;; Caro's files on her local host
 (load "~/Documents/2014spring/cs61as/cs61as_SICP_schython/obj.scm")
 (load "~/Documents/2014spring/cs61as/cs61as_SICP_schython/parser.scm")
 (load "~/Documents/2014spring/cs61as/cs61as_SICP_schython/py-primitives.scm")
 (load "~/Documents/2014spring/cs61as/cs61as_SICP_schython/py-meta.scm")
 (initialize-python)

|#
