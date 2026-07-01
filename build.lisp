(require :asdf)

;; Force ASDF to only look here for systems.
(asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :ignore-inherited-configuration))

(format t "--- LOADING SYSTEM ---~%")
(declaim (optimize (speed 3) (debug 1) (safety 1)))
(asdf:load-system :vend)

(format t "--- COMPILING EXECUTABLE ---~%")
(setf uiop:*image-entry-point* #'vend:main)
(uiop:dump-image #p"vend"
                 :executable t
                 :compression #+sbcl (not (null (member :sb-core-compression *features*)))
                              #-sbcl nil)
(format t "--- DONE ---~%")
(uiop:quit 0)
