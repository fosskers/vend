;;; The running of the program itself.

(in-package :vend)

;; --- Constants --- ;;

(defparameter +require-asdf+ "(require \"asdf\")")
(defparameter +init-registry+ "(asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :ignore-inherited-configuration))")

;; --- Graph --- ;;

(defun scan-systems! (graph paths)
  "Given a collection of paths to asd files, extract all their system definitions
and mutably add them to a given graph. As a return value, yields the keyword
names of the added systems."
  (t:transduce (t:comp #++(t:log (lambda (acc path) (vlog "Reading ~a" path)))
                       (t:map #'systems-from-file)
                       #'t:concatenate
                       (t:map (lambda (sys)
                                (let ((name (system-name sys)))
                                  (g:add-node! graph name)
                                  (dolist (dep (depends-from-system sys))
                                    (g:add-node! graph dep)
                                    (g:add-edge! graph name dep))
                                  name))))
               #'t:cons paths))

#+nil
(systems-from-file #p"/home/colin/code/common-lisp/CIEL/vendored/cmd/cmd.asd")

(defun add-extra-deps! (graph parent deps)
  "When we somehow know a fixed list of dependencies to associate with a given
parent, add them to the graph. One instance of where this can occur is when a
package is known to use PIS, and its dependencies are hard-coded, even though no
originally system could be found for it."
  (dolist (dep deps)
    (g:add-node! graph dep)
    (g:add-edge! graph parent dep)))

(defun vend/graph (&key focus)
  "Produce a dependency graph of all systems depended upon by systems of the root
project. If FOCUS is supplied, only considers the subgraph with that FOCUS as
the root."
  (let ((graph (build-graph (uiop:getcwd) focus)))
    (with-open-file (stream #p"deps.dot" :direction :output :if-exists :supersede)
      (g:to-dot-with-stream graph stream))))

(defun build-graph (path &optional focus)
  "From the given path, scan systems and build a dependency graph. Assumes you have
a populated `vendored/' directory already."
  (let* ((graph (g:make-graph))
         (top   (scan-systems! graph (root-asd-files path))))
    (scan-systems! graph (asd-files (f:join path "vendored")))
    ;; This accounts for the PIS workaround.
    (t:transduce (t:comp (t:map #'car)
                         (t:filter (lambda (sys) (getf +pis+ sys))))
                 (t:for (lambda (sys) (add-extra-deps! graph sys (getf +pis+ sys))))
                 (g:graph-nodes graph))
    (cond (focus (g:subgraph graph (into-keyword focus)))
          (top (apply #'g:subgraph graph top))
          (t (error "No top-level systems found in ~a" path)))))

#+nil
(g:graph-edges (build-graph #p"/home/colin/code/common-lisp/CIEL/"))

;; --- Search --- ;;

(defun vend/search (term)
  "Check the names of all known systems for a given term."
  (let* ((matches (t:transduce (t:comp (t:map (lambda (pair)
                                                (cons (keyword->string (car pair))
                                                      (cdr pair))))
                                       (t:filter (lambda (pair) (substring? (car pair) term))))
                               #'t:cons (t:plist +sources+)))
         (longest (t:transduce (t:map (lambda (pair) (length (car pair))))
                               (t:fold #'max 0) matches)))
    (dolist (pair matches)
      (format t "~va ~a~%" longest (car pair) (cdr pair)))))

#++
(vend/search "nonexistent")

;; --- Check --- ;;

(defun vend/check (&key focus)
  "Check the dependency graph for old deps, etc."
  (let* ((graph (g:make-graph))
         (top   (scan-systems! graph (root-asd-files (uiop:getcwd)))))
    (scan-systems! graph (asd-files (f:join (uiop:getcwd) "vendored")))
    (let ((final (cond (focus (g:subgraph graph (into-keyword focus)))
                       (t (apply #'g:subgraph graph top)))))
      (t:transduce (t:comp (t:map #'car)
                           (t:filter-map (lambda (sys)
                                           (let ((parent (get-parent sys)))
                                             (cond ((member parent +deprecated+) (cons :deprecated sys))
                                                   ((member parent +missing+) (cons :missing sys))))))
                           (t:map (lambda (pair)
                                    (let* ((sys (cdr pair))
                                           (reason (car pair))
                                           (routes  (g:paths-to final sys))
                                           (longest (reverse (cdr (t:transduce (t:map (lambda (route) (cons (length route) route)))
                                                                               (t:fold (lambda (a b) (if (> (car a) (car b)) a b)))
                                                                               routes)))))
                                      (case reason
                                        (:deprecated (format t "~a is deprecated.~%" (bold-red sys)))
                                        (:missing (format t "~a is not publically available and so was not vendored!~%" (bold-red sys))))
                                      (format t "  ~{~a~^ -> ~}~%" longest)))))
                   #'t:for-each (g:graph-nodes final)))))

#++
(vend/check)

;; --- Downloading --- ;;

(defun clone (url path)
  "Given a source URL to clone from, do a shallow git clone into a given absolute PATH."
  (unless (probe-file path)
    (multiple-value-bind (output error-output code)
        (uiop:run-program (list "git" "clone" "--quiet" "--depth=1" url path)
                          :output t
                          :error-output t
                          :ignore-error-status t)
      (declare (ignore output error-output))
      (assert (= 0 code) nil "Clone failed: ~a" url))))

(defun work (cwd target)
  "Recursively perform a git clone on every detected dependency."
  (let ((graph  (g:make-graph))
        (cloned (make-hash-table)))
    (labels ((unique-leaves (g)
               (t:transduce (t:comp (t:map (lambda (leaf) (or (get-parent leaf) leaf)))
                                    #'t:unique
                                    (t:filter (lambda (leaf)
                                                (not (or (gethash leaf cloned)
                                                         (member leaf +exclude+)
                                                         (member leaf +missing+))))))
                            #'t:cons (g:leaves g)))
             (recurse (top dep)
               (unless (gethash dep cloned)
                 (let* ((url  (getf +sources+ dep))
                        (path (f:ensure-string (f:join target (keyword->string dep)))))
                   (cond
                     ;; We've already cloned this repository. But note that we
                     ;; still do a system scan, just in case we've cloned this
                     ;; particular repo, but perhaps not its dependencies yet.
                     ((probe-file path)
                      (vlog "Fetched  ~a" (bold dep))
                      (scan-systems! graph (asd-files path)))
                     ;; We don't know of this dependency, but it might be using PIS.
                     ((not url) (let ((deps (getf +pis+ dep)))
                                  (when (null deps)
                                    (let ((route (reverse (car (g:paths-to graph dep)))))
                                      (error "~a is not a known system.~%~%  ~{~a~^ -> ~}~%~%Please have it registered in the vend source code." (bold-red dep) route)))
                                  (add-extra-deps! graph dep deps)))
                     ;; We're clear to attempt a fresh clone.
                     (t (vlog "Fetching ~a" (bold dep))
                        (clone url path)
                        (scan-systems! graph (asd-files path))))
                   (setf (gethash dep cloned) t)
                   (dolist (leaf (unique-leaves (apply #'g:subgraph graph top)))
                     (recurse top leaf))))))
      (let* ((top  (scan-systems! graph (root-asd-files cwd)))
             (root (t:transduce (t:comp (t:map #'get-parent)
                                        #'t:unique)
                                #'t:first top)))
        ;; This is the root project directory, so it's already considered "cloned".
        (setf (gethash root cloned) t)
        (dolist (leaf (unique-leaves graph))
          (recurse top leaf))))))

#++
(let* ((cwd #p"/home/colin/code/common-lisp/cl-tuition/")
       (dir (f:ensure-directory (f:join cwd "vendored"))))
  (work cwd dir))

;; --- Project Initialization --- ;;

(defparameter +defsystem-template+
  "(defsystem \"~a\"
  :version \"0.0.0\"
  :author \"\"
  :license \"\"
  :homepage \"\"
  :depends-on ()
  :serial t
  :components ((:module \"src\" :components ((:file \"package\"))))
  :description \"\")
")

(defparameter +defpackage-template+
  "(defpackage ~a
  (:use :cl)
  (:documentation \"\"))

(in-package :~a)
")

(defparameter +gitignore-template+
  "vendored/
*.fasl
*.fas
")

(defun vend/init (name)
  "Given the name of a project, create a simple directory structure with a minimal .asd file."
  (ensure-directories-exist (f:ensure-directory (f:join name "src")))
  (with-open-file (f (f:join name (f:with-extension name "asd"))
                     :direction :output
                     :if-does-not-exist :create)
    (format f +defsystem-template+ name))
  (with-open-file (f (f:join name "src" "package.lisp")
                     :direction :output
                     :if-does-not-exist :create)
    (format f +defpackage-template+ name name))
  (with-open-file (f (f:join name ".gitignore")
                     :direction :output
                     :if-does-not-exist :create)
    (format f +gitignore-template+)))

;; --- Executable --- ;;

(defparameter +help+
  "vend - Vendor your Common Lisp dependencies

Commands:
  check  [focus] - Check your dependencies for issues
  eval   [sexps] - Evaluate SEXPs in the current project
  get            - Download all project dependencies into 'vendored/'
  graph  [focus] - Visualise a graph of transitive project dependencies
  init   [name]  - Create a minimal project skeleton
  repl   [args]  - Start a Lisp session with only your vendored ASDF systems
  search [term]  - Search known systems
  test   [args]  - Run all detected test systems

Flags:
  --help    - Display this help message
  --version - Display the current version of vend
")

(defun vend/help ()
  (princ +help+))

(defun vend/get ()
  "Download all dependencies."
  (let* ((cwd (uiop:getcwd))
         (dir (f:ensure-directory (f:join cwd "vendored"))))
    (vlog "Downloading dependencies.")
    (handler-bind ((error (lambda (c)
                            (format t "~a~%" c)
                            (uiop:quit 1))))
      (work cwd dir))
    (vlog "Done.")))

(defun vend/test (args &key (dir (uiop:getcwd)))
  "Run detected test systems."
  (let* ((compiler (or (car args) "sbcl"))
         (eval (eval-flag compiler))
         (clisp (if (clisp? compiler) '("-repl") '()))
         (systems (t:transduce (t:comp (t:map #'systems-from-file)
                                       #'t:concatenate)
                               #'t:cons (root-asd-files dir)))
         (tests (test-invocations systems)))
    (when tests
      (let ((exps (t:transduce (t:comp (t:intersperse eval)
                                       (t:once eval))
                               #'t:cons (append (list +require-asdf+ +init-registry+) tests))))
        (vlog "Running tests.")
        (multiple-value-bind (output error-output code)
            (uiop:run-program (cons compiler (append (cdr args) clisp exps))
                              :output t
                              :error-output t
                              :ignore-error-status t)
          (declare (ignore output error-output))
          (unless (zerop code)
            (uiop:quit 1)))))))

#++
(vend/test '() :dir #p"/home/colin/code/common-lisp/filepaths/")

(defun vend/repl (args)
  "Start a given repl."
  (cond ((compiler? (car args)) (evaluate (car args) (cdr args)))
        (t (evaluate "sbcl" args))))

(defun vend/eval (args)
  "Evaluate some Lisp in the context of the local project, then exit."
  (multiple-value-bind (compiler priority extra) (eval-args args)
    (evaluate compiler priority extra)))

(defun eval-args (args)
  "Form the CLI args necessary for an `--eval' call."
  (multiple-value-bind (compiler sexps)
      (cond ((compiler? (car args)) (values (car args) (cadr args)))
            (t (values "sbcl" (car args))))
    (let* ((eval  (eval-flag compiler))
           (split (t:transduce (t:comp #'t:sexp (t:intersperse eval) (t:once eval)) #'t:cons sexps))
           ;; NOTE: 2025-08-01 If the user passed no s-expressions, we must
           ;; remove the `--eval' added by the `once' call above, since it's the
           ;; only thing left in the list.
           (split (if (null (cdr split)) '() split)))
      (multiple-value-bind (priority extra) (extra-flags compiler)
        (values compiler priority (append split extra))))))

#+nil
(eval-args '("ecl" "(+ 1 1)"))
#+nil
(eval-args '("(+ 1 1)"))
#+nil
(eval-args '("ecl"))

(defun evaluate (compiler args &optional extra)
  "Evaluate some Lisp in the requested REPL."
  (let* ((eval (eval-flag compiler))
         (load (list eval +require-asdf+ eval +init-registry+))
         (clisp (if (clisp? compiler) '("-repl") '())))
    (uiop:run-program (cons compiler (append args clisp load extra))
                      :output :interactive
                      :error-output :interactive
                      :input :interactive
                      :ignore-error-status t)))

(defun vend/usage-error (format-control &rest args)
  "Report a command-line usage error and exit with the normal Unix code 2."
  (format *error-output* "~&vend: ")
  (apply #'format *error-output* format-control args)
  (format *error-output* "~%~%")
  (vend/help)
  (uiop:quit 2))

(defun args-after-double-dash (args)
  "Return the command-line arguments after a \"--\" marker, if one exists."
  (loop for tail on args
        when (string= "--" (car tail))
          return (cdr tail)))

(defun vend/command-line-arguments ()
  "Return user arguments in a form suitable for `vend/dispatch'.

When a Lisp image is launched through an implementation's script wrapper, UIOP
may include a leading \"--\" marker.  A saved executable normally does not.  The
normalised list lets the dispatcher treat both forms the same way."
  (let ((args (uiop:command-line-arguments)))
    (cond ((and args (string= "--" (car args))) (cdr args))
          (args args)
          #+lispworks
          (t (args-after-double-dash system:*line-arguments-list*))
          #-lispworks
          (t nil))))

(defun vend/dispatch (args)
  "Run the vend command represented by ARGS.

ARGS is a list of user-facing strings, such as '(\"graph\" \"transducers\").
The dispatcher is intentionally plain: each command is visible in one place,
and commands with optional arguments pass NIL when the argument is absent."
  (let ((command (car args))
        (rest (cdr args)))
    (cond ((null command) (vend/help))
          ((member command '("--help" "-h") :test #'string=) (vend/help))
          ((string= command "--version") (format t "0.3.3~%"))
          ((string= command "check") (vend/check :focus (car rest)))
          ((string= command "eval") (vend/eval rest))
          ((string= command "get") (vend/get))
          ((string= command "graph") (vend/graph :focus (car rest)))
          ((string= command "init")
           (if (car rest)
               (vend/init (car rest))
               (vend/usage-error "missing project name for `vend init'")))
          ((string= command "repl") (vend/repl rest))
          ((string= command "search")
           (if (car rest)
               (vend/search (car rest))
               (vend/usage-error "missing search term for `vend search'")))
          ((string= command "test") (vend/test rest))
          (t (vend/usage-error "unknown command: ~a" command)))))

(defun main ()
  (vend/dispatch (vend/command-line-arguments))
  (uiop:quit 0))

;; Bad boys:
;; https://github.com/slyrus/opticl/blob/master/opticl-doc.asd
;; https://github.com/rpav/fast-io/blob/master/fast-io-test.asd
;; https://github.com/sharplispers/ironclad/blob/master/ironclad.asd
;; https://github.com/sharplispers/chipz/blob/master/chipz.asd
;; https://github.com/rpav/fast-io/blob/master/fast-io.asd#L15
