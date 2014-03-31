(defpackage #:someshell
  (:nicknames #:ss)
  (:use :common-lisp :cl-ppcre)
  (:export #:shell-reader%
           #:load-aliases
           #:save-aliases
           #:return-aliases
           #:alias-add
           #:range
           #:iota))
