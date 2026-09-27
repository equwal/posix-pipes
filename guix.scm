;;; guix.scm --- Guix package for posix-pipes.  Build with: guix build -f guix.scm
;;; Install with: guix package -f guix.scm
(use-modules (guix packages) (guix gexp) (guix build-system asdf)
             ((guix licenses) #:prefix license:)
             (gnu packages lisp) (gnu packages lisp-xyz) (gnu packages lisp-check))

(define %source-dir (dirname (current-filename)))

(define-public sbcl-posix-pipes
  (package
    (name "sbcl-posix-pipes")
    (version "0.0.1")
    (source (local-file %source-dir "posix-pipes-checkout"
                        #:recursive? #t
                        #:select? (lambda (file stat)
                                    (not (or (string-suffix? ".fasl" file)
                                             (string-contains file "/.git"))))))
    (build-system asdf-build-system/sbcl)
    (arguments (list #:asd-systems ''("posix-pipes")))
    (inputs (list))
    (synopsis "Portably run a command with input/output to/from CL")
    (description "Portably run a command with input/output to/from CL.")
    (home-page "https://github.com/equwal/posix-pipes")
    (license license:lgpl2.1)))

(define-public cl-posix-pipes
  (sbcl-package->cl-source-package sbcl-posix-pipes))

sbcl-posix-pipes
