(require :asdf)

;; Force ASDF to only look here for systems.
(asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :ignore-inherited-configuration))

(format t "--- LOADING SYSTEM ---~%")
(declaim (optimize (speed 3) (debug 1) (safety 1) (space 3)))

#+ecl
(asdf:load-system :vend)

(format t "--- COMPILING EXECUTABLE ---~%")
#-ecl (asdf:make :vend)
#+ecl (asdf:make-build :vend :type :program :move-here #p"./" :epilogue-code '(vend:main))
(format t "--- DONE ---~%")
(quit)
