;;; The parsing of ASD files and extraction of System data.

(in-package :vend)

(defun components-less? (a b)
  "Comparison of path components."
  (labels ((recurse (ac bc)
             (cond ((null ac) t)
                   ((and (not (null ac)) (null bc)) nil)
                   ((and (null (cdr ac)) (not (null (cdr bc)))) t)
                   ((and (not (null (cdr ac))) (null (cdr bc))) nil)
                   ((and (null (cdr ac)) (null (cdr bc)))
                    (string-lessp (f:base (car ac))
                                  (f:base (car bc))))
                   ((string-lessp (car ac) (car bc)) t)
                   ((string-lessp (car bc) (car ac)) nil)
                   (t (recurse (cdr ac) (cdr bc))))))
    (recurse (f:components a) (f:components b))))

#++
(components-less? "trivial-gray-streams.asd" "trivial-gray-streams-test.asd")
#++
(components-less? #p"/home/colin/foo" #p"/home/colin/foo/bar")
#++
(components-less? #p"/home/colin/foo/bar" #p"/home/colin/foo")

(defun asd-files (dir &key (shallow nil))
  "Yield the pathnames of all `.asd' files found in the given DIR."
  (let* ((patt (if shallow "*.asd" "**/*.asd"))
         (uniq (t:transduce (t:unique-by #'f:name) #'t:snoc (directory (f:join dir patt) :resolve-symlinks nil))))
    (sort uniq #'components-less?)))

#++
(asd-files "./")
#+nil
(asd-files "/home/colin/code/common-lisp/transducers/vendored/parachute/")

(defun root-asd-files (dir)
  "Yield the pathnames of all `.asd' outside of `vendored/'."
  (t:transduce (t:comp (t:filter (lambda (path)
                                   (let ((dir (car (last (f:components path)))))
                                     (and (not (string= ".git" dir))
                                          (not (string= "vendored" dir))
                                          (not (string= ".qlot" dir))))))
                       (t:map #'asd-files)
                       (t:once (asd-files dir :shallow t))
                       #'t:concatenate)
               #'t:cons (directory (f:ensure-directory (f:join dir "*/")))))

#++
(root-asd-files (ext:getcwd))
#++
(root-asd-files "/home/colin/code/common-lisp/zauberwald/")

(declaim (ftype (function (string) boolean) comment?))
(defun comment? (string)
  "Is the given STRING a comment line?"
  (unless (zerop (length string))
    (char= #\; (char string 0))))

#++
(comment? "")
#++
(comment? "; Hello!")

(defun chipz? (string)
  "A hack to account for `chipz.asd'."
  (or (string-starts-with? string "#+chipz-system:")
      (string-starts-with? string "#-chipz-system:")))

#++
(chipz? "#+chipz-system:gray-streams")

(defun string-from-file (path)
  "Preserves newlines but removes whole-line comments."
  (t:transduce (t:comp (t:filter (lambda (line) (not (comment? line))))
                       (t:filter (lambda (line) (not (chipz? (string-left-trim " " line)))))
                       (t:intersperse '(#\Newline))
                       #'t:concatenate)
               #'t:string path))

#++
(string-from-file #p"vend.asd")

(fn systems-from-file (-> pathname list))
(defun systems-from-file (path)
  "Extract all `defsystem' forms as proper sexp from a file."
  (t:transduce (t:map (lambda (sys)
                        (let* ((clean (sanitize sys))
                               (stream (make-string-input-stream clean)))
                          (read stream nil :eof))))
               #'t:cons (all-system-strings (string-from-file path))))

#++
(systems-from-file (car (asd-files "./")))

(defun reader-macro? (chars)
  (and (eql #\# (nth 0 chars))
       (eql #\. (nth 1 chars))
       (eql #\( (nth 2 chars))))

#++
(reader-macro? (coerce "#.(+ 1 1)" 'list))
#++
(reader-macro? (coerce "(+ 1 1)" 'list))
#++
(reader-macro? (coerce "" 'list))

(defun other-reader? (chars)
  "A hack for Postmodern."
  (and (eql #\# (nth 0 chars))
       (eql #\. (nth 1 chars))))

(defun asdf-call? (chars)
  (and (eql #\( (nth 0 chars))
       (eql #\a (nth 1 chars))
       (eql #\s (nth 2 chars))
       (eql #\d (nth 3 chars))
       (eql #\f (nth 4 chars))
       (eql #\: (nth 5 chars))))

#++
(asdf-call? (coerce "(asdf:foo)" 'list))
#++
(asdf-call? (coerce "(asdf" 'list))
#++
(asdf-call? (coerce "(foo)" 'list))

(defun command-asdf-call? (chars)
  (and (eql #\space (nth 0 chars))
       (eql #\a (nth 1 chars))
       (eql #\s (nth 2 chars))
       (eql #\d (nth 3 chars))
       (eql #\f (nth 4 chars))
       (eql #\: (nth 5 chars))))

(defun quoted-asdf-call? (chars)
  (and (eql #\' (nth 0 chars))
       (eql #\a (nth 1 chars))
       (eql #\s (nth 2 chars))
       (eql #\d (nth 3 chars))
       (eql #\f (nth 4 chars))
       (eql #\: (nth 5 chars))))

(defun uiop-call? (chars)
  (and (eql #\( (nth 0 chars))
       (eql #\u (nth 1 chars))
       (eql #\i (nth 2 chars))
       (eql #\o (nth 3 chars))
       (eql #\p (nth 4 chars))
       (eql #\: (nth 5 chars))))

;; For StumpVM.
(defun uiop-package-call? (chars)
  (and (eql #\( (nth 0 chars))
       (eql #\u (nth 1 chars))
       (eql #\i (nth 2 chars))
       (eql #\o (nth 3 chars))
       (eql #\p (nth 4 chars))
       (eql #\/ (nth 5 chars))
       (eql #\p (nth 6 chars))
       (eql #\a (nth 7 chars))
       (eql #\c (nth 8 chars))
       (eql #\k (nth 9 chars))
       (eql #\a (nth 10 chars))
       (eql #\g (nth 11 chars))
       (eql #\e (nth 12 chars))
       (eql #\: (nth 13 chars))))

#+nil
(uiop-package-call? (coerce "(uiop/package:symbol-call)" 'list))

(defun grovel-call? (chars)
  (and (eql #\( (nth 0 chars))
       (eql #\c (nth 1 chars))
       (eql #\f (nth 2 chars))
       (eql #\f (nth 3 chars))
       (eql #\i (nth 4 chars))
       (eql #\- (nth 5 chars))
       (eql #\g (nth 6 chars))
       (eql #\r (nth 7 chars))
       (eql #\o (nth 8 chars))
       (eql #\v (nth 9 chars))
       (eql #\e (nth 10 chars))
       (eql #\l (nth 11 chars))
       (eql #\: (nth 12 chars))))

(defun checkl? (chars)
  (and (eql #\( (nth 0 chars))
       (eql #\c (nth 1 chars))
       (eql #\h (nth 2 chars))
       (eql #\e (nth 3 chars))
       (eql #\c (nth 4 chars))
       (eql #\k (nth 5 chars))
       (eql #\l (nth 6 chars))
       (eql #\: (nth 7 chars))))

(defun at-defsystem? (str ix)
  "Does the string at the given index start with a defsystem form?"
  (and (or (string-starts-with? str "(defsystem " :from ix)
           (string-starts-with? str "(asdf:defsystem " :from ix))
       (or (zerop ix)
           ;; Due to Ironclad's ridiculous ASD file.
           (eql #\newline (aref str (1- ix))))))

#++
(at-defsystem? "(defsystem :foo)" 0)
#++
(at-defsystem? " (defsystem :foo)" 1)
#++
(at-defsystem? " (asdf:defsystem :foo)" 1)
#++
(at-defsystem? "(push 1 2)" 0)
#++
(at-defsystem? "`(defsystem :foo)" 1)

(defun extract-defsystem (str ix)
  "Given a starting index into a string, and the knowledge that a `defsystem' form
starts there, extract that substring. As a second return value, yields the index
to continue from."
  (let ((len (length str)))
    (labels ((find-closing-paren (parens curr-ix)
               (let ((char (aref str curr-ix)))
                 (cond ((>= curr-ix len) nil)
                       ((eql #\( char) (find-closing-paren (1+ parens) (1+ curr-ix)))
                       ((and (eql #\) char) (= 1 parens)) curr-ix)
                       ((eql #\) char) (find-closing-paren (1- parens) (1+ curr-ix)))
                       (t (find-closing-paren parens (1+ curr-ix)))))))
      (let ((end (find-closing-paren 1 (1+ ix))))
        (when end
          (values (subseq str ix (1+ end))
                  (1+ end)))))))

#++
(extract-defsystem "(defsystem :foo :depends-on (:a :b :c)) (next)" 0)
#++
(extract-defsystem " (defsystem :foo :depends-on (:a :b :c)) (next)" 1)
#++
(extract-defsystem "" 0)

(defun all-system-strings (str)
  "Extract all `defsystem' forms as substrings from some parent string."
  (let ((len (length str)))
    (labels ((recurse (acc ix)
               (cond ((>= ix len) acc)
                     ((at-defsystem? str ix)
                      (multiple-value-bind (sub next) (extract-defsystem str ix)
                        (recurse (cons sub acc) next)))
                     (t (recurse acc (1+ ix))))))
      (nreverse (recurse '() 0)))))

#++
(all-system-strings "(foo) (defsystem :bar) (push 1 2) (defsystem :baz) t")
#++
(all-system-strings "")

;; FIXME: 2025-08-23 All this kind of sucks. I'd like there to be a more
;; principled way to clean up these system definitions, or just read the
;; dependencies directly.
(defun sanitize (str)
  "Remove and/or replace a number of naughty forms that prevent `read' from
succeeding as-is on a given string."
  (labels ((keep (acc chars)
             (let ((head (car chars))
                   (tail (cdr chars)))
               (cond ((null head) acc)
                     ;; The presence of read-time macros confuses `read', so we
                     ;; proactively remove them.
                     ((reader-macro? chars)
                      (keep (cons #\t acc) (chuck 1 (cddr tail))))
                     ((other-reader? chars)
                      (keep acc (cdr tail)))
                     ;; Likewise, some clever package authors like to utilise
                     ;; `asdf' and `uiop' directly in their system definitions.
                     ;; This similarly causes problems with `read', so we remove
                     ;; such calls.
                     ((or (asdf-call? chars)
                          (uiop-call? chars))
                      (keep (cons #\( acc) (nthcdr 5 tail)))
                     ((uiop-package-call? chars)
                      (keep (cons #\( acc) (nthcdr 13 tail)))
                     ((quoted-asdf-call? chars)
                      (keep (cons #\' acc) (nthcdr 5 tail)))
                     ((command-asdf-call? chars)
                      (keep (cons #\space acc) (nthcdr 5 tail)))
                     ((grovel-call? chars)
                      (keep (cons #\t acc) (chuck 1 (cddr tail))))
                     ((checkl? chars)
                      (keep (cons #\t acc) (chuck 1 (cddr tail))))
                     (t (keep (cons head acc) tail)))))
           (chuck (parens chars)
             (let ((head (car chars)))
               (cond ((zerop parens) chars)
                     ((null head) '())
                     ((eql #\( head) (chuck (1+ parens) (cdr chars)))
                     ((eql #\) head) (chuck (1- parens) (cdr chars)))
                     (t (chuck parens (cdr chars)))))))
    (coerce (reverse (keep '() (coerce str 'list)))
            'string)))

#++
(sanitize "(asdf:defsystem :foo
:long-description #.(+ 1 1)
:foo (asdf:bar)
:baz (cffi-grovel:grovel-file 1)
:beep (checkl:tests 1)
:fruit #.*yes*
:zoo (uiop/package:symbol-call 1)
:action asdf:do-it)")

(defun depends-from-system (sexp)
  "Extract the `:depends-on' list from a sexp, if it has one."
  (t:transduce (t:filter-map
                (lambda (dep)
                  (etypecase dep
                    (keyword dep)
                    (string (into-keyword dep))
                    (symbol (into-keyword dep))
                    (list (destructuring-bind (kw a &optional b) dep
                            (cond ((eq :version kw) (into-keyword a))
                                  ((eq :require kw) (into-keyword a))
                                  ;; HACK: To account for Radiance's
                                  ;; higher-level implementation injection
                                  ;; mechanism. `:interface' is otherwise not a
                                  ;; keyword supported by ASDF.
                                  ((eq :interface kw) nil)
                                  ((eq :feature kw)
                                   (typecase b
                                     (list (into-keyword (cadr b)))
                                     (t    (into-keyword b))))
                                  (t (error "Unknown composite dependency declaration: ~a" dep))))))))
               #'t:snoc
               (getf sexp :depends-on)))

#++
(depends-from-system (car (sexps-from-file (car (asd-files "/home/colin/code/common-lisp/trial/vendored/com-inuoe-jzon/")))))

(defun system-name (sexp)
  (let ((name (nth 1 sexp)))
    (etypecase name
      (keyword name)
      (string (into-keyword name))
      (symbol (into-keyword (symbol-name name))))))

;; --- Testing Systems --- ;;

#++
(system-name (car (sexps-from-file (car (asd-files "./")))))
#++
(system-name '(defsystem foo))

(defun test-op? (list)
  (and (string-equal "TEST-OP" (symbol-name (car list)))
       (string-equal "TEST-OP" (symbol-name (car (nth 1 list))))))

#++
(test-op? '(test-op (test-op :foobar)))

(defun extract-test-op (list)
  (nth 1 (nth 1 list)))

#++
(extract-test-op '(test-op (test-op :foobar)))

(defun extract-symbol-call-args (list)
  "Extract the arguments to `symbol-call' from a `:perform' entry."
  (let ((body (nth 2 list)))
    (when (string-equal "SYMBOL-CALL" (symbol-name (car body)))
      (cdr body))))

#++
(extract-symbol-call-args '(asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.alloy.test)))

(defun testable-systems (systems)
  "Find the keyword names of systems for which `asdf:test-system' would function."
  (t:transduce (t:comp (t:filter-map (lambda (sys)
                                       (let ((name (system-name sys)))
                                         (t:transduce (t:comp (t:filter #'test-op?)
                                                              (t:map (lambda (op)
                                                                       (cons name (into-keyword (extract-test-op op))))))
                                                      #'t:cons (getf sys :in-order-to)))))
                       #'t:concatenate)
               #'t:cons systems))

(defun test-invocations (systems)
  "From some systems, extract testable systems and produce a series of sexp
strings, which if passed to `--eval', would result in the test suites running
while intelligently catching failures."
  (t:transduce (t:comp (t:unique-by #'cdr)
                       (t:filter-map (lambda (pair)
                                       (let ((system (t:transduce #'t:pass (t:find (lambda (sys) (eq (cdr pair) (system-name sys)))) systems)))
                                         (unless system
                                           (error "~a specifies a test-op whose associated system does not exist!" (bold-red (car pair))))
                                         (cons (car pair) system))))
                       (t:map (lambda (pair)
                                (destructuring-bind (name . sys) pair
                                  (let ((deps (depends-from-system sys)))
                                    ;; NOTE: Add support for other testing libraries here.
                                    (cond ((member :parachute deps)
                                           (parachute-test (system-name sys)
                                                           (nth 2 (extract-symbol-call-args (getf sys :perform)))))
                                          ((member :clunit2 deps) (clunit2-test (system-name sys)))
                                          ((member :fiveam deps) (fiveam-test (system-name sys)))
                                          (t (list (asdf-test-system name))))))))
                       #'t:concatenate)
               #'t:cons (testable-systems systems)))

#++
(let* ((systems (t:transduce (t:comp (t:map #'systems-from-file)
                                     #'t:concatenate)
                             #'t:cons (root-asd-files #p"/home/colin/code/common-lisp/alloy/"))))
  (test-invocations systems))

(defun parachute-test (sys pkg)
  "Generate the structure of a parachute test from a given system name."
  (let ((pkg (or pkg sys)))
    (list (format nil "(asdf:load-system :~a)" sys)
          (format nil "
(let* ((status (parachute:status (parachute:test :~a)))
       (code (if (eq :passed status) 0 1)))
  (uiop:quit code))
" pkg))))

#++
(let* ((status (parachute:status (parachute:test :~a)))
       (code (if (eq :passed status) 0 1)))
  (uiop:quit code))

(defun asdf-test-system (sys)
  "Just a normal `test-system' call."
  (format nil "(progn (asdf:test-system :~a) (uiop:quit 0))" sys))

(defun fiveam-test (sys)
  "Generate the structure of a fiveam test from a given system name."
  (list (format nil "(asdf:load-system :~a)" sys)
        (format nil "
(multiple-value-bind (status obj foo) (~a:all-tests)
  (declare (ignore obj foo))
  (let ((code (if status 0 1)))
    (uiop:quit code)))
" sys)))

#++
(multiple-value-bind (status obj foo) (~a:all-tests)
  (declare (ignore obj foo))
  (let ((code (if status 0 1)))
    (uiop:quit code)))

(defun clunit2-test (sys)
  "Generate the structure of a clunit2 test from a given system name."
  (list (format nil "(asdf:load-system :~a)" sys)
        (format nil "
(handler-bind ((clunit::test-suite-failure (lambda (c) (declare (ignore c)) (uiop:quit 1))))
  (clunit:run-all-suites :signal-condition-on-fail t :stop-on-fail t)
  (uiop:quit 0))
" sys)))

#++
(handler-bind ((clunit::test-suite-failure (lambda (c) (declare (ignore c)) (uiop:quit 1))))
  (clunit:run-all-suites :signal-condition-on-fail t :stop-on-fail t)
  (uiop:quit 0))
