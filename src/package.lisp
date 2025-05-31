(defpackage vend
  (:use :cl)
  (:local-nicknames (#:g #:simple-graph)
                    (#:p #:filepaths)
                    (#:t #:transducers)
                    (#:x #:parcom/xml))
  (:export #:main)
  (:documentation "Simply vendor your Common Lisp project dependencies."))

(in-package :vend)

#-ecl
(error "VEND can only be compiled with ECL.")

;; --- Strings --- ;;

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

(defun clisp? (compiler)
  "Is this clisp?"
  (string-equal "clisp" compiler))

(defun eval-flag (compiler)
  "The flag necessary to directly inject Lisp into a new REPL."
  (cond ((string-equal "alisp" compiler) "-e")
        ((clisp? compiler) "-x")
        (t "--eval")))
