;;;; package.lisp

(defpackage #:posix-pipes
  (:use #:cl)
  (:export :pipe-output
           :pipe-input
           :close-pipe
           :with-open-pipe)
