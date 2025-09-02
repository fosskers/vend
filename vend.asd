(defsystem "vend"
  :version "0.3.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0" :homepage "https://github.com/fosskers/vend"
  :depends-on (:filepaths :simple-graph :transducers) 
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "implementation-specific")
                             (:file "registry")
                             (:file "asd")
                             (:file "vend"))))
  :description "Simply vendor your Common Lisp project dependencies."
  :build-operation program-op
  :build-pathname "vend"
  :entry-point "vend:main"
  )
