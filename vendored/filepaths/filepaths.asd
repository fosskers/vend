(defsystem "filepaths"
  :version "1.0.2"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "LGPL-3.0-only"
  :depends-on ()
  :components ((:module "src" :components ((:file "filepaths"))))
  :description "Modern and consistent filepath manipulation."
  :in-order-to ((test-op (test-op :filepaths/tests))))

(defsystem "filepaths/tests"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "LGPL-3.0-only"
  :depends-on (:filepaths :parachute)
  :components ((:module "tests" :components ((:file "tests"))))
  :description "Test system for filepaths"
  :perform (test-op (op c) (symbol-call :parachute :test :filepaths/tests)))
