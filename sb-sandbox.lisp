;;;
;;; sandbox.lisp -- specifications for the sandboxed language.
;;;

(defpackage :sb-sandbox
  (:use :cl))

(in-package :sb-sandbox)

(defconstant +allowed-functions+ '(+ - * /))
(defconstant +allowed-macros+    '(defun defmacro defvar))
(defvar *defined-symbols* nil)
