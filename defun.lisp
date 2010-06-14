;;;
;;; defun.lisp - definitions of the `%defun' function from src/code/defboot.lisp
;;;

(in-package :sb-impl)

(defmacro define-%defun ()
 `(defun %defun (name def doc inline-lambda source-location)
    (declare (type function def))
    (declare (type (or null simple-string) doc))
    (aver (legal-fun-name-p name))
    (sb-c:%compiler-defun name inline-lambda nil)
    (when (fboundp name)
      (style-warn 'sb-kernel::redefinition-with-defun :name name
                  :old (fdefinition name) :new def
                  :new-location source-location))
    (setf (fdefinition name) def)
    (push name sb-sandbox::*defined-symbols*) ;; <<--
    (sb-c::note-name-defined name :function)
    (when doc (setf (%fun-doc def) doc))
    name))

(defmacro restore-%defun ()
 `(defun %defun (name def doc inline-lambda source-location)
    (declare (type function def))
    (declare (type (or null simple-string) doc))
    (aver (legal-fun-name-p name))
    (sb-c:%compiler-defun name inline-lambda nil)
    (when (fboundp name)
      (style-warn 'sb-kernel::redefinition-with-defun :name name
                  :old (fdefinition name) :new def
                  :new-location source-location))
    (setf (fdefinition name) def)
    (sb-c::note-name-defined name :function)
    (when doc (setf (%fun-doc def) doc))
    name))
