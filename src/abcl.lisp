;;; Integrations custom to ABCL and the provisioning of JVM packages from Maven
;;; Central. Compatible with the syntax expected by `abcl-asdf', namely:
;;;
;;; ```
;;; (asdf:defsystem :log4j
;;;   :components ((:mvn "log4j/log4j" :version "1.4.9"))
;;; ```
;;;
;;; Many functions here thus operate on a `dep' plist of the shape:
;;;
;;; ```
;;; (:group ... :artifact ... :version ...)
;;; ```

(in-package :vend)

(defun maven-deps (system)
  "Given a `defsystem' sexp, extract its Maven Central dependencies, if any."
  (t:transduce
   (t:comp (t:filter (lambda (pl) (and (getf pl :mvn) (getf pl :version))))
           (t:map (lambda (pl)
                    (destructuring-bind (group artifact)
                        (t::string-split (getf pl :mvn) :separator #\/)
                      (list :group group
                            :artifact artifact
                            :version (getf pl :version))))))
   #'t:cons
   (getf system :components)))

#+nil
(maven-deps '(asdf:defsystem :log4j
              :components ((:mvn "log4j/log4j" :version "1.4.9"))))

(defun jar-download-command (dep)
  "Produce the command necessary to download something from Maven Central."
  (list "mvn" "dependency:get"
        (format nil "-DgroupId=~a" (getf dep :group))
        (format nil "-DartifactId=~a" (getf dep :artifact))
        (format nil "-Dversion=~a" (getf dep :version))
        "-Dmaven.repo.local=vendored/java/"))

#+nil
(jar-download-command
 (car (maven-deps '(asdf:defsystem :log4j
                    :components ((:mvn "log4j/log4j" :version "1.4.9"))))))

(defun download-from-maven (dep)
  (let ((cmd (jar-download-command dep)))
    (multiple-value-bind (stream code obj)
        (ext:run-program (car cmd) (cdr cmd) :output *standard-output*)
      (declare (ignore stream obj))
      (assert (= 0 code) nil "Pulling ~a from Maven Central failed" (getf dep :mvn)))))

#+nil
(download-from-maven '(:mvn "org.apache.commons/commons-text" :version "1.13.1"))

(defun java-dep-dir (dep)
  "Rederive a directory path in which a JAR and POM can be found."
  (let* ((group    (getf dep :group))
         (artifact (getf dep :artifact))
         (version  (getf dep :version))
         (parts    (t::string-split group :separator #\.)))
    (p:ensure-directory (apply #'p:join "vendored" "java" (append parts (list artifact version))))))

#+nil
(java-dep-dir '(:group "org.apache.commons" :artifact "commons-text" :version "1.13.1"))

(defun java-jar-path (dep)
  "The expected path to a downloaded JAR of this dep."
  (let ((jar (format nil "~a-~a.jar" (getf dep :artifact) (getf dep :version))))
    (p:join (java-dep-dir dep) jar)))

#+nil
(java-jar-path '(:group "org.apache.commons" :artifact "commons-text" :version "1.13.1"))

(defun java-pom-path (dep)
  "The expected path to a downloaded POM of this dep."
  (let ((pom (format nil "~a-~a.pom" (getf dep :artifact) (getf dep :version))))
    (p:join (java-dep-dir dep) pom)))

#+nil
(java-pom-path '(:group "org.apache.commons" :artifact "commons-text" :version "1.13.1"))

;; NOTE: 2025-05-25 This is about 2x faster than `uiop:read-file-string', and
;; much, much faster than reading it in line-by-line and re-fusing via
;; transducers.
(declaim (ftype (function (pathname) (simple-array character *)) string-from-file))
(defun string-from-file (path)
  "Read some given file into a single string."
  (with-open-file (stream path :direction :input :element-type 'character)
    (let* ((len (file-length stream))
           (str (make-string len)))
      (read-sequence str stream)
      str)))
