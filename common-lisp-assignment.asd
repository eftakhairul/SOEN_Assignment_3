;;;; 2013-11-16 02:16:11
;;;;
;;;; Think of this as your project file.
;;;; Keep it up to date, and you can reload your project easily
;;;;  by right-clicking on it and selecting "Load Project"

(defpackage #:common-lisp-assignment-asd
  (:use :cl :asdf))

(in-package :common-lisp-assignment-asd)

(defsystem common-lisp-assignment
  :name "common-lisp-assignment"
  :version "0.1"
  :serial t
  :components ((:file "defpackage")
               (:file "main" :depends-on ("defpackage"))
               
               
               ; As you add files to your project,
               ; make sure to add them here as well
               
               )
  :depends-on ())
