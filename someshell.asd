;;; someshell.asd

(asdf:defsystem #:someshell
  :serial t
  :description "A shell thingy"
  :author "Riley Eltrich"
  :license "GPLv3+"
  :depends-on (:cl-ppcre)
  :components ((:file "package")
               (:file "someshell")))
