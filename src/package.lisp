(defpackage vend
  (:use :cl)
  (:local-nicknames (#:g #:simple-graph)
                    (#:p #:filepaths)
                    (#:t #:transducers))
  (:export #:main)
  (:documentation "Simply vendor your Common Lisp project dependencies."))

(in-package :vend)

#-ecl
(error "VEND can only be compiled with ECL.")

;; --- Strings --- ;;

;; FIXME: 2025-07-28 Use `string=' instead. Benchmark the difference first though.
(defun string-starts-with? (string prefix &key (from 0))
  (let ((pos (mismatch prefix string :start2 from)))
    (or (null pos)
        (>= pos (length prefix)))))

#++
(string-starts-with? "trial-alloy" "trial-")
#++
(string-starts-with? "hello" "b")
#++
(string-starts-with? "hello" "llo" :from 2)

(defun substring? (string sub)
  "Is one string a substring of another?"
  (not (null (search sub string))))

#++
(substring? "hello" "ll")
#++
(substring? "hello" "all")

(defun into-keyword (s)
  "Turn anything stringy or symboly into a keyword."
  (etypecase s
    (keyword s)
    (string (intern (string-upcase s) "KEYWORD"))
    (symbol (intern (symbol-name s) "KEYWORD"))))

#++
(into-keyword 'foo)

(defun keyword->string (kw)
  (t:transduce (t:map (lambda (c) (if (equal #\. c) #\-  c)))
               #'t:string (string-downcase (format nil "~a" kw))))

#++
(keyword->string :KW)
#++
(keyword->string :com.inuoe.jzon)

;; --- Colours --- ;;

(defun bold-red (text)
  "Highlight some text in red."
  (format nil "~c[31;1m~a~c[0m" #\escape text #\escape))

(defun bold-cyan (text)
  "Highlight some text in cyan."
  (format nil "~c[96;1m~a~c[0m" #\escape text #\escape))

(defun bold (text)
  "Just enbolden some text without colouring it."
  (format nil "~c[1m~a~c[0m" #\escape text #\escape))

;; --- Logging --- ;;

(defun vlog (text &rest rest)
  (format t "~a " (bold-cyan "[vend]"))
  (apply #'format t text rest)
  (format t "~%"))

;; --- Compiler --- ;;

(defparameter *compilers* '("sbcl" "ecl" "abcl" "alisp" "clasp" "ccl" "clisp"))

(defun clisp? (compiler)
  "Is this clisp?"
  (string= "clisp" compiler))

(defun compiler? (arg)
  "Does the given CLI arg refer to a known compiler?"
  (member arg *compilers* :test #'string=))

(defun eval-flag (compiler)
  "The flag necessary to directly inject Lisp into a new REPL."
  (cond ((string= "alisp" compiler) "-e")
        ((clisp? compiler) "-x")
        (t "--eval")))

(defun extra-flags (compiler)
  "Extra flags to pass to the compiler. The first list is for 'priority' flags that
must come before any '--eval' flags."
  (cond ((string= "sbcl" compiler)  (values '("--noinform" "--non-interactive") '()))
        ((string= "ecl" compiler)   (values '() '("--eval" "(ext:quit 0)")))
        ((string= "abcl" compiler)  (values '("--noinform") '("--eval" "(ext:quit)")))
        ((string= "alisp" compiler) (values '() '("--kill")))
        ((string= "clisp" compiler) (values '("--silent") '("-x" "(ext:quit)")))
        ((string= "ccl" compiler)   (values '() '("--eval" "(ccl:quit)")))))

