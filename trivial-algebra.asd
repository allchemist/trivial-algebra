(defpackage :trivial-algebra-system
    (:use #:common-lisp #:asdf))

(in-package :trivial-algebra-system)

(defsystem trivial-algebra
  :name "Trivial Linear Algebra in Lisp"
  :description "Several vector, matrix and numeric operations in pure lisp."
  :licence "Public Domain"
  :version "0.3"
  :author "Khokhlov Ivan"
  :components ((:file "package")
	       (:file "vectors" :depends-on ("package"))
	       (:file "matrix" :depends-on ("package"))
	       (:file "quaternions" :depends-on ("package" "vectors"))))
