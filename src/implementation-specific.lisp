(in-package :vend)

(defun run-program (program-and-args &key (input t) (output t))
  (format t "~a~%" program-and-args)
  #+ecl (ext:run-program (car program-and-args) (cdr program-and-args) :input input :output output)
  #+sbcl (sb-ext:run-program (car program-and-args) (cdr program-and-args) :search t :wait t :input input :output output)
  )

(defun quit (code)
  #+ecl (ext:exit code) 
  #+sbcl (sb-ext:quit :unix-status code)
  )

(defun getcwd ()
  #+ecl (ext:getcwd)
  #+sbcl (sb-posix:getcwd)
  )

(defun command-line-arguments ()
  #+ecl (cdr ext:*command-args*)
  #+sbcl (cdr sb-ext:*posix-argv*)
  )
