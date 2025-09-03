;;;; pulled straight from CLOCC

(in-package #:posix-pipes)

(defun pipe-output (prog &rest args)
  "Return an output stream which will go to the command."
  #+allegro (excl:run-shell-command (format nil "~a~{ ~a~}" prog args)
                                    :input :stream :wait nil)
  #+clisp (ext:make-pipe-output-stream (format nil "~a~{ ~a~}" prog args))
  #+cmu (ext:process-input (ext:run-program prog args :input :stream
                                                      :output t :wait nil))
  #+gcl (si::fp-input-stream (apply #'si:run-process prog args))
  #+lispworks (sys::open-pipe (format nil "~a~{ ~a~}" prog args)
                              :direction :output)
  #+lucid (lcl:run-program prog :arguments args :wait nil :output :stream)
  #+sbcl (sb-ext:process-input (sb-ext:run-program prog args :input :stream
                                                             :output t :wait nil))
  #-(or allegro clisp cmu gcl lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'pipe-output prog args)))

(defun pipe-input (prog &rest args)
  "Return an input stream from which the command output will be read."
  #+allegro (excl:run-shell-command (format nil "~a~{ ~a~}" prog args)
                                    :output :stream :wait nil)
  #+clisp (ext:make-pipe-input-stream (format nil "~a~{ ~a~}" prog args))
  #+cmu (ext:process-output (ext:run-program prog args :output :stream
                                                       :error t :input t :wait nil))
  #+gcl (si::fp-output-stream (apply #'si:run-process prog args))
  #+lispworks (sys::open-pipe (format nil "~a~{ ~a~}" prog args)
                              :direction :input)
  #+lucid (lcl:run-program prog :arguments args :wait nil :input :stream)
  #+sbcl (sb-ext:process-output (sb-ext:run-program prog args :output :stream
                                                              :error t :input t :wait nil))
  #-(or allegro clisp cmu gcl lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'pipe-input prog args)))

;;; Allegro CL: a simple `close' does NOT get rid of the process.
;;; The right way, of course, is to define a Gray stream `pipe-stream',
;;; define the `close' method and use `with-open-stream'.
;;; Unfortunately, not every implementation supports Gray streams, so we
;;; have to stick with this to further the portability.
;;; [2005] actually, all implementations support Gray streams (see gray.lisp)
;;; but Gray streams may be implemented inefficiently

(defun close-pipe (stream)
  "Close the pipe stream."
  (declare (stream stream))
  (close stream)
  ;; CLOSE does not close constituent streams
  ;; CLOSE-CONSTRUCTED-STREAM:ARGUMENT-STREAM-ONLY
  ;; http://www.lisp.org/HyperSpec/Issues/iss052.html
  (typecase stream
    (two-way-stream
     (close (two-way-stream-input-stream stream))
     (close (two-way-stream-output-stream stream))))
  #+allegro (sys:reap-os-subprocess))

(defmacro with-open-pipe ((pipe open) &body body)
  "Open the pipe, do something, then close it."
  `(let ((,pipe ,open))
     (declare (stream ,pipe))
     (unwind-protect (progn ,@body)
       (close-pipe ,pipe))))
