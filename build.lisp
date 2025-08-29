(require :asdf)

;; Force ASDF to only look here for systems.
(asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :ignore-inherited-configuration))

(format t "--- LOADING SYSTEM ---~%")
(declaim (optimize (speed 3) (debug 1) (safety 1)))


(progn
  (format t "--- COMPILING EXECUTABLE ---~%")
  (asdf:make :vend/executable)
  (format t "--- DONE ---~%")
  (uiop:quit 0))
