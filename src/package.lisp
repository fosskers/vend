(defpackage vend
  (:use :cl)
  (:local-nicknames (#:g #:simple-graph)
                    (#:f #:filepaths)
                    (#:t #:transducers))
  (:export #:main)
  (:documentation "Simply vendor your Common Lisp project dependencies."))

(in-package :vend)

#-ecl
(error "VEND can only be compiled with ECL.")

;; --- Typing --- ;;

(defmacro fn (name type)
  "A shorthand for declaiming function types."
  `(declaim (ftype ,type ,name)))

(deftype -> (a b &rest args)
  "A shorthand for function types."
  (if (null args)
      `(function (,a) ,b)
      (let ((argz (butlast args))
            (res (car (last args))))
        `(function (,a ,b ,@argz) ,res))))

;; --- Strings --- ;;

(declaim (ftype (function (string string &key (:from fixnum)) boolean) string-starts-with?))
(defun string-starts-with? (s prefix &key (from 0))
  (string= prefix s :start2 from :end2 (min (+ from (length prefix))
                                            (length s))))

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
  "Get the string of keyword in a form suitable for becoming a filename."
  (t:transduce (t:map (lambda (c) (if (equal #\. c) #\-  c)))
               #'t:string (string-downcase (symbol-name kw))))

#++
(keyword->string :KW)
#++
(keyword->string :com.inuoe.jzon)

;; --- Colours --- ;;

(defparameter *colour?* t
  "Whether or not to add ANSI colour codes to STDOUT messages. This is not to
  have `setq` called on it; instead you should bind it locally with `let` and
  have that naturally propagate through the child calls down to where it's
  needed, namely in `vlog`, etc.")

(defun colour? ()
  "Should STDOUT messages contain colour codes?"
  (let ((var (ext:getenv "NO_COLOR")))
    (not (or (string-equal var "yes")
             (string-equal var "1")
             (string-equal var "true")))))

(defun bold-red (text)
  "Highlight some text in red."
  (cond (*colour?* (format nil "~c[31;1m~a~c[0m" #\escape text #\escape))
        (t text)))

(defun bold-cyan (text)
  "Highlight some text in cyan."
  (cond (*colour?* (format nil "~c[96;1m~a~c[0m" #\escape text #\escape))
        (t text)))

(defun bold (text)
  "Just enbolden some text without colouring it."
  (cond (*colour?* (format nil "~c[1m~a~c[0m" #\escape text #\escape))
        (t text)))

;; --- Logging --- ;;

(defun vlog (text &rest rest)
  (format t "~a " (bold-cyan "[vend]"))
  (apply #'format t text rest)
  (format t "~%"))

;; --- Compiler --- ;;

(defparameter *compilers* '("sbcl" "ecl" "abcl" "alisp" "clasp" "ccl" "clisp" "cmucl"))

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
        ((string= "ccl" compiler)   (values '() '("--eval" "(ccl:quit)")))
        ((string= "cmucl" compiler) (values '("--quiet") '("--eval" "(quit)")))))
