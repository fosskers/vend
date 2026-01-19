(defpackage filepaths
  (:use :cl)
  ;; --- Structural tests --- ;;
  (:export #:root? #:rootp #:empty? #:emptyp
           #:starts-with? #:starts-with-p #:ends-with? #:ends-with-p
           #:absolute? #:absolutep #:relative? #:relativep
           #:directory? #:directoryp)
  ;; --- Construction --- ;;
  (:export #:join)
  ;; --- Component Access --- ;;
  (:export #:base #:with-base
           #:name #:with-name
           #:parent #:with-parent
           #:extension #:with-extension #:drop-extension #:add-extension)
  ;; --- Conversion --- ;;
  (:export #:components #:from-list
           #:ensure-directory #:ensure-string #:ensure-path
           #:to-string #:from-string)
  ;; --- Conditions --- ;;
  (:export #:no-filename
           #:empty-path
           #:root-no-parent)
  (:documentation "Modern and consistent filepath manipulation."))

(in-package :filepaths)

(defconstant +empty-path+ #p"")
(defconstant +filesystem-root+ #p"/")
(defconstant +separator+ #\/)

(defmacro rootp (path)
  `(root? ,path))

(declaim (ftype (function ((or pathname string)) boolean) root?))
(defun root? (path)
  "Is the given PATH the root directory?"
  (or (and (stringp path)
           (string-equal "/" path))
      (and (pathnamep path)
           (equal +filesystem-root+ path))))

#+nil
(rootp #p"/")

(defmacro emptyp (path)
  `(empty? ,path))

(declaim (ftype (function ((or pathname string)) boolean) empty?))
(defun empty? (path)
  "Is the given PATH an empty string?"
  (or (and (stringp path)
           (= 0 (length path)))
      (and (pathnamep path)
           (equal +empty-path+ path))))

#+nil
(emptyp #p"")

(defmacro starts-with-p (path base)
  `(starts-with? ,path ,base))

(defun starts-with? (path base)
  "Are the initial components of a PATH some BASE?"
  (let ((bools (mapcar #'equal (components path) (components base))))
    (reduce (lambda (a b) (and a b)) bools :initial-value t)))

#+nil
(starts-with-p #p"/foo/bar/baz/zing.json" "/foo/bar")

(defmacro ends-with-p (path child)
  `(ends-with? ,path ,child))

(defun ends-with? (path child)
  "Are the final components of a PATH some given CHILD?"
  (let ((bools (mapcar #'equal
                       (reverse (components path))
                       (reverse (components child)))))
    (reduce (lambda (a b) (and a b)) bools :initial-value t)))

#+nil
(ends-with-p #p"/foo/bar/baz/zing.json" "baz/zing.json")

(defmacro absolutep (path)
  `(absolute? ,path))

(declaim (ftype (function ((or pathname string)) boolean) absolute?))
(defun absolute? (path)
  "Yields T when the given PATH is a full, absolute path."
  (if (pathnamep path)
      (eq :absolute (car (pathname-directory path)))
      (and (< 0 (length path))
           (equal +separator+ (char path 0)))))

#+nil
(absolutep "/home/colin/foo.txt")

(defmacro relativep (path)
  `(relative? ,path))

(declaim (ftype (function ((or pathname string)) boolean) relative?))
(defun relative? (path)
  "Yields T when the given PATH is a relative one."
  (not (absolutep path)))

#+nil
(relativep #p"/home/colin/foo.txt")
#+nil
(relativep #p"foo.txt")

(defmacro directoryp (path)
  `(directory? ,path))

(declaim (ftype (function ((or pathname string)) boolean) directory?))
(defun directory? (path)
  "Yields T if the PATH represents a directory.

Note that this only checks the formatting of the path, and does not query the
filesystem."
  (if (pathnamep path)
      (and (not (null (pathname-directory path)))
           (not (pathname-name path)))
      (equal +separator+ (char path (1- (length path))))))

#+nil
(directory? "/foo/bar/")
#+nil
(directory? "/foo/bar/baz.txt")

(declaim (ftype (function ((or pathname string)) simple-string) base))
(defun base (path)
  "The non-extension, non-directory portion of the filename of a PATH."
  (let ((b (pathname-name path)))
    (if (not b)
        (error 'no-filename :path path)
        (string-if-keyword-impl-specific b))))

#+nil
(base "/foo/bar/baz.txt")
#+nil
(base #p"/foo/bar/ゆびわ.txt")

(declaim (ftype (function ((or pathname string) string) pathname) with-base))
(defun with-base (path new)
  "Swap the base portion of a PATH with a NEW one. Yields a new path object."
  (let ((path (ensure-path path)))
    (make-pathname :name new
                   :type (pathname-type path)
                   :device (pathname-device path)
                   :directory (pathname-directory path)
                   :version :newest)))

#+nil
(with-base #p"/foo/bar/baz.txt" "jack")

(declaim (ftype (function ((or pathname string)) simple-string) name))
(defun name (path)
  "The filename of a PATH with no other directory components."
  (let ((n (file-namestring path)))
    (if (= 0 (length n))
        (error 'no-filename :path path)
        n)))

#+nil
(name "/foo/bar/baz.txt")

(declaim (ftype (function ((or pathname string) (or pathname string)) pathname) with-name))
(defun with-name (path new)
  "Swap the filename portion of a PATH with a NEW one. Yields a new path object."
  (let ((path (ensure-path path)))
    (make-pathname :name (base new)
                   :type (extension new)
                   :device (pathname-device path)
                   :directory (pathname-directory path)
                   :version :newest)))

#+nil
(with-name #p"/foo/bar/baz.txt" "jack.json")

(declaim (ftype (function ((or pathname string)) pathname) parent))
(defun parent (path)
  "Yield PATH without its final component, if there is one."
  (cond ((emptyp path) (error 'empty-path))
        ((rootp path)  (error 'root-no-parent))
        (t (let* ((s (ensure-string path))
                  (path (if (directoryp s)
                            (string-right-trim "/" s)
                            s)))
             (from-string (directory-namestring path))))))

#+nil
(parent "/foo/bar/baz.txt")
#+nil
(parent "/foo/bar/")
#+nil
(parent "/foo/")

(declaim (ftype (function ((or pathname string) (or pathname string)) pathname) with-parent))
(defun with-parent (path parent)
  "Swap the parent portion of a PATH."
  (join parent (name path)))

#+nil
(with-parent #p"/foo/bar/baz.json" #p"/zing")

(declaim (ftype (function ((or pathname string)) (or simple-string keyword null)) extension))
(defun extension (path)
  "The extension of a given PATH."
  (pathname-type path))

#+nil
(extension #p"/foo/bar.json")
#+nil
(extension #p"/")
#++
(extension #p"*.*")

(declaim (ftype (function ((or pathname string) string) pathname) with-extension))
(defun with-extension (path ext)
  "Swap the entire extension of a given PATH. Yields a new path object."
  (let ((path (ensure-path path)))
    (if (directoryp path)
        (error 'no-filename :path path)
        (make-pathname :name (base path)
                       :type ext
                       :device (pathname-device path)
                       :directory (pathname-directory path)
                       :version :newest))))

#+nil
(with-extension #p"/foo/bar/baz.txt" "json")
#+nil
(with-extension #p"/foo/bar/" "json")

(declaim (ftype (function ((or pathname string)) pathname) drop-extension))
(defun drop-extension (path)
  "Remove an extension from a PATH."
  (let* ((path (ensure-path path))
         (stem (base path))
         (ext  (extension stem))
         (name (if ext (base stem) stem)))
    ;; Similar to `add-extension', there's some cleverness here where we need to
    ;; check if we must move an "inner" extension back outward. For instance, in
    ;; the case of foo.json.zip. It looks like we're always setting `:type' to
    ;; something concrete, but this will properly be NIL in the normal case
    ;; where only one extension was present.
    (make-pathname :name name
                   :type ext
                   :device (pathname-device path)
                   :directory (pathname-directory path)
                   :version :newest)))

#+nil
(drop-extension #p"/foo/bar/baz.json")
#+nil
(drop-extension #p"/foo/bar/baz.json.zip")

(declaim (ftype (function ((or pathname string) string) pathname) add-extension))
(defun add-extension (path ext)
  "Add an extension to the given path, even if it already has one."
  (let* ((path    (ensure-path path))
         (already (extension path)))
    (if already
        ;; The pathname type only wants a single extension present in the
        ;; `:type' field, or else there is strange behaviour elsewhere (for
        ;; instance involving `to-string'). The old extension must thus become
        ;; part of the stem, and the only value reported by `extension' is the
        ;; new one, not the composite. This behaviour reflects that of Rust's
        ;; standard library.
        (make-pathname :name (concatenate 'string (base path) "." already)
                       :type ext
                       :device (pathname-device path)
                       :directory (pathname-directory path)
                       :version :newest)
        (with-extension path ext))))

#+nil
(add-extension #p"/foo/bar/baz.txt" "zip")

(declaim (ftype (function ((or pathname string) &rest (or pathname string)) pathname) join))
(defun join (parent &rest children)
  "Combine one or more components together."
  (let* ((parent     (ensure-path parent))
         (cleaned    (remove-if (lambda (s)
                                  (or (string= +separator+ s)
                                      (string= "" s)))
                                (mapcan #'components children)))
         (combined   (append (components parent) cleaned))
         (final      (car (last combined)))
         (rest       (butlast combined))
         (absolute?  (absolute? parent))
         (abs-or-rel (if absolute? :absolute :relative))
         (final-base (base final))
         (dir?       (or (and (null cleaned) (directory? parent))
                         (and cleaned (directory? (car (last children))))))
         (version    (if dir? nil :newest)))
    (make-pathname :name (cond
                           (dir? nil)
                           #+sbcl
                           ((string= "**" final-base) (sbcl-wildcard))
                           #+cmucl
                           ((string= "**" final-base) (cmucl-wildcard))
                           #+(or abcl ccl allegro)
                           ((string= "**" final-base) final-base)
                           (t (keyword-if-special final-base)))
                   :type (extension final)
                   ;; NOTE: 2025-12-29 The `equal` behaviour for pathnames is
                   ;; different between SBCL and ECL. The latter seems to take
                   ;; `:version' into account, while the former does not.
                   ;;
                   ;; Directories should have a NIL `:version', as that field is
                   ;; intended to refer to versions of the file itself on the
                   ;; filesystem; a meaningless notion for a directory.
                   :version version
                   :device (pathname-device parent)
                   :directory (cons abs-or-rel
                                    (mapcar #'keyword-if-special
                                            (cond ((and dir? absolute?) (cdr combined))
                                                  (absolute? (cdr rest))
                                                  (dir? combined)
                                                  (t rest)))))))

#+nil
(join "foo" "baz" "bar/")

#+nil
#p"**.json"

#+nil
(join "/foo" "**.json")
#+nil
(join "/" "foo" "bar" ".." "." ".." "baz" "stuff.json")
#+nil
#p"/foo/bar/.././../baz/stuff.json"

#+nil
(join "/foo" "bar" "**" "*.json")

#+nil
(join "/foo" "bar" "**.json")

#++
(join "/foo/" "*.*")

#+nil
(join "/foo" "bar" "baz" "test.json")
#+nil
(join "/foo/bar" "baz/test.json")

(declaim (ftype (function ((or pathname string)) list) components))
(defun components (path)
  "Every component of a PATH broken up as a list."
  (cond ((empty? path) '())
        ;; HACK 2024-06-17 Until ECL/Clasp support `**' in `:name' position.
        ;;
        ;; And not even a good hack, since it can be broken in cases where the
        ;; `**' comes at the end.
        #+(or ecl clasp)
        ((and (stringp path) (string= "**" path)) '("**"))
        (t (let* ((path (ensure-path path))
                  (comp (mapcar #'string-if-keyword (directory-parts path)))
                  (list (if (directoryp path)
                            comp
                            (let* ((ext  (extension path))
                                   (file (if ext
                                             (concatenate 'string (base path) "."
                                                          (string-if-keyword ext))
                                             (base path))))
                              (append comp (list file))))))
             (if (absolutep path)
                 (cons "/" list)
                 list)))))

#+nil
(components "/foo/bar/baz.json")
#+nil
(components "/foo/bar/.././../baz/stuff.json")
#+nil
(components "/foo/bar/./baz/stuff.json")
#++
(components "foo/*.*")
#+nil
(components ".")
#+nil
(components "/.")
#+nil
(components "foo/.")
#+nil
#p"foo/."

#+nil
(pathname-directory #p"foo/bar/baz")
#+nil
(pathname-directory #p"./")

(defun directory-parts (path)
  "Light post-processing around `pathname-directory' to ensure sanity."
  (let ((parts (pathname-directory path)))
    (if (and (eq :relative (car parts))
             (null (cdr parts)))
        '(".")
        (cdr parts))))

#+nil
(directory-parts #p"/foo/bar/baz.txt")
#+nil
(directory-parts #p"/.")
#+nil
(directory-parts #p"foo/.")
#+nil
(directory-parts #p"foo/./")

(declaim (ftype (function (list) pathname) from-list))
(defun from-list (list)
  "Given a LIST of path components, construct a proper pathname object."
  (if (null list)
      #p""
      (destructuring-bind (parent &rest rest) list
        (if (null rest)
            (ensure-path parent)
            (apply #'join parent (car rest) (cdr rest))))))

#+nil
(from-list '("foo" "bar" "baz"))
#+nil
(from-list '("foo" "bar" "." "baz"))

(declaim (ftype (function ((or pathname string)) pathname) ensure-directory))
(defun ensure-directory (path)
  "If a given PATH doesn't end in a path separator, add one."
  (let ((path (ensure-path path)))
    (if (directoryp path)
        path
        (make-pathname :name nil
                       :type nil
                       :device (pathname-device path)
                       ;; NOTE: 2025-05-12 The result of `pathname-directory'
                       ;; will be nil when a relative path with a single
                       ;; component is given. In that case, we must help it
                       ;; yield the correct structure to be appended to
                       ;; immediately afterward.
                       :directory (append (or (pathname-directory path)
                                              '(:relative))
                                          (list (keyword-if-special (name path))))))))

#+nil
(ensure-directory #p"/foo/bar/baz")
#+nil
(ensure-directory "/foo")
#+nil
(ensure-directory "foo")
#+nil
(ensure-directory #p"/foo/bar/*")
#+nil
(ensure-directory #p"/foo/bar/**")

(declaim (ftype (function ((or pathname string)) simple-string) ensure-string))
(defun ensure-string (path)
  "A PATH is definitely a string after this."
  (if (pathnamep path) (to-string path) path))

(declaim (ftype (function (pathname) simple-string) to-string))
(defun to-string (path)
  "Convert a PATH object into string."
  (namestring path))

(declaim (ftype (function ((or pathname string)) pathname) ensure-path))
(defun ensure-path (path)
  "A PATH is definitely a pathname after this."
  (if (pathnamep path) path (from-string path)))

#+nil
(ensure-path ".")
#+nil
(ensure-path "/.")
#+nil
(ensure-path "foo/bar/.")
#+nil
(ensure-path "/foo/./bar/foo.txt")

;; NOTE: 2025-08-17 Unfortunately, CMUCL can't be stopped from stripping "."
;; components from the directory field. Even if I manually add them back in a
;; `make-pathname', it still strips them. This effect trickles up to
;; `components', which suffers if a dot exists in the input.
(declaim (ftype (function (string) pathname) from-string))
(defun from-string (s)
  "Convert a string into a proper filepath object."
  (pathname s))

#+nil
(from-string ".")

#+nil
(from-string "foo/.")

#+nil
(join "/" "foo" "bar" ".." "." ".." "baz" "stuff.json")

#+nil
(pathname "/foo/bar/.././../baz/test.json")

#+nil
(pathname "/foo/bar/../baz/test.json")

#+nil
#p"/foo/bar/.././../baz/test.json"

;; --- Conditions --- ;;

(define-condition no-filename (error)
  ((path :initarg :path :reader no-filename-path))
  (:documentation "The given path was expected to be a filename, but it may have been a directory
or empty string instead.")
  (:report (lambda (condition stream)
             (format stream "The given path was expected to be a filename: ~a"
                     (no-filename-path condition)))))

(define-condition empty-path (error)
  ()
  (:documentation "A non-empty path was expected, but an empty one was given.")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Empty path given where a concrete one was expected."))))

(define-condition root-no-parent (error)
  ()
  (:documentation "The filesystem root has no parent.")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "The filesystem root has no parent."))))

;; --- Utilities --- ;;

(declaim (ftype (function (t) string) string-if-keyword-impl-specific))
(defun string-if-keyword-impl-specific (item)
  "Like `string-if-keyword' but with special consideration for implementations.
Assumed to be used internally within `base' to account for when the file base is
a wildcard character."
  (cond
    #+sbcl
    ((sb-impl::pattern-p item) "**") ; FIXME 2024-06-16 Actually check the contents.
    #+cmucl
    ((lisp::pattern-p item) "**")
    (t (string-if-keyword item))))

(declaim (ftype (function ((or string keyword)) string) string-if-keyword))
(defun string-if-keyword (item)
  "There are certain keywords that represent special path components. These need to
be converted back into their original string representations if present."
  (cond ((or (eq item :up) (eq item :back)) "..")
        ((eq item :wild) "*")
        ((eq item :wild-inferiors) "**")
        (t item)))

#+nil
(string-if-keyword :up)

(declaim (ftype (function (string) (or string keyword)) keyword-if-special))
(defun keyword-if-special (item)
  "Like `string-if-keyword', certain strings need to be converted to keywords
before being stored in the `:directory' portion of a pathname."
  (cond ((string-equal ".." item) :up)
        ((string-equal "**" item) :wild-inferiors)
        ((string-equal "*" item)  :wild)
        (t item)))

#+sbcl
(defun sbcl-wildcard ()
  "A SBCL-specific pattern type created when a ** appears in a path."
  (sb-impl::make-pattern '(:multi-char-wild :multi-char-wild)))

#+cmucl
(defun cmucl-wildcard ()
  "A CMUCL-specific pattern type created when a ** appears in a path."
  (lisp::make-pattern '(:multi-char-wild :multi-char-wild)))
