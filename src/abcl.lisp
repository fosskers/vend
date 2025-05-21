;;; Integrations custom to ABCL and the provisioning of JVM packages from Maven
;;; Central. Compatible with the syntax expected by `abcl-asdf', namely:
;;;
;;; (asdf:defsystem :log4j
;;;   :components ((:mvn "log4j/log4j" :version "1.4.9"))

(in-package :vend)

(defun maven-deps (system)
  "Given a `defsystem' sexp, extract its Maven Central dependencies, if any."
  (t:transduce (t:filter (lambda (pl) (and (getf pl :mvn) (getf pl :version))))
               #'t:cons
               (getf system :components)))

#+nil
(maven-deps '(asdf:defsystem :log4j
              :components ((:mvn "log4j/log4j" :version "1.4.9"))))

(defun form-download-command (dep)
  "Produce the command necessary to download something from Maven Central."
  (destructuring-bind (group artifact)
      (t::string-split (getf dep :mvn) :separator #\/)
    (list "mvn" "dependency:get"
          (format nil "-DgroupId=~a" group)
          (format nil "-DartifactId=~a" artifact)
          (format nil "-Dversion=~a" (getf dep :version))
          "-Dmaven.repo.local=vendored/java/")))

#+nil
(form-download-command
 (car (maven-deps '(asdf:defsystem :log4j
                    :components ((:mvn "log4j/log4j" :version "1.4.9"))))))

(defun download-from-maven (dep)
  (let ((cmd (form-download-command dep)))
    (multiple-value-bind (stream code obj)
        (ext:run-program (car cmd) (cdr cmd) :output *standard-output*)
      (declare (ignore stream obj))
      (assert (= 0 code) nil "Pulling ~a from Maven Central failed" (getf dep :mvn)))))

#+nil
(download-from-maven '(:mvn "commons-io/commons-io" :version "2.16.1"))
