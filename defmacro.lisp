;;;
;;; defmacro.lisp - definitions of the `%defmacro' function from src/code/defmacro.lisp
;;;

(in-package :sb-c)

(defmacro define-%defmacro ()
 `(defun %defmacro (name definition lambda-list doc debug-name)
    ,@(unless set-p
        '((declare (ignore lambda-list debug-name))))
    (ecase (info :function :kind name)
      ((nil))
      (:function
       (undefine-fun-name name)
       (style-warn
          "~S is being redefined as a macro when it was ~
                 previously ~(~A~) to be a function."
          name
          (info :function :where-from name)))
        (:macro)
        (:special-form
         (error "The special form ~S can't be redefined as a macro."
                name)))
      (clear-info :function :where-from name)
      (setf (macro-function name) definition)
      (push name sb-sandbox::*defined-macros*) ;; <<--
      ,(when set-p
         `(setf (%fun-doc definition) doc
                (%fun-lambda-list definition) lambda-list
                (%fun-name definition) debug-name))
      name)))

(defmacro undefine-%defmacro ()
 `(let ((*package* (find-package :sb-c)))
    (defun %defmacro (name definition lambda-list doc debug-name)
      ,@(unless set-p
          '((declare (ignore lambda-list debug-name))))
      (ecase (info :function :kind name)
        ((nil))
        (:function
         (undefine-fun-name name)
         (style-warn
          "~S is being redefined as a macro when it was ~
                 previously ~(~A~) to be a function."
          name
          (info :function :where-from name)))
        (:macro)
        (:special-form
         (error "The special form ~S can't be redefined as a macro."
                name)))
      (clear-info :function :where-from name)
      (setf (macro-function name) definition)
      ,(when set-p
         `(setf (%fun-doc definition) doc
                (%fun-lambda-list definition) lambda-list
                (%fun-name definition) debug-name))
      name)))
