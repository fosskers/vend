(defsystem "vend"
  :version "0.3.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0" :homepage "https://github.com/fosskers/vend"
  :depends-on (:filepaths :simple-graph :transducers :asdf :uiop)
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "registry")
                             (:file "asd")
                             (:file "vend"))))
  :description "Simply vendor your Common Lisp project dependencies."
  )



(asdf:defsystem "vend/executable"
  :depends-on ("vend")
  :build-operation program-op
  :build-pathname "vend" ;; shell name
  :entry-point "vend:main" ;; thunk
  )

