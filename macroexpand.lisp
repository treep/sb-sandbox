;;;
;;; macroexpand.lisp -- reimplementation functions from src/code/macroexpand.lisp
;;;

(in-package :sb-impl)

(setf *macroexpand-hook*
      #'(lambda (def form env &optional name)
          (if name
              (if (or (member name sb-sandbox::+allowed-macros+)   ;; <<--
                      (member name sb-sandbox::*defined-symbols*)) ;; <<--
                  (funcall def form env)
                  (error "~A is not in allowed or defined symbols" name)))
          (funcall def form env)))

(defun macroexpand-1* (form &optional env)
  (cond ((and (consp form) (symbolp (car form)))
         (let ((def (macro-function (car form) env)))
           (if def
               (values (funcall *macroexpand-hook* def form (coerce-to-lexenv env) (car form)) t)
               (values form nil))))
        ((symbolp form)
         (flet ((perform-symbol-expansion (symbol expansion)
                  (funcall *macroexpand-hook* (constantly expansion) symbol env form)))
           (let* ((venv (when env (sb-c::lexenv-vars env)))
                  (local-def (cdr (assoc form venv))))
             (cond ((and (consp local-def)
                         (eq (car local-def) 'macro))
                    (values (perform-symbol-expansion form (cdr local-def)) t))
                   (local-def
                    (values form nil))
                   ((eq (info :variable :kind form) :macro)
                    (let ((expansion (info :variable :macro-expansion form)))
                      (values (perform-symbol-expansion form expansion) t)))
                   (t
                    (values form nil))))))
        (t
         (values form nil))))

(defun macroexpand* (form &optional env)
  (labels ((frob (form expanded)
             (multiple-value-bind (new-form newly-expanded-p)
                 (macroexpand-1* form env)
               (if newly-expanded-p
                   (frob new-form t)
                   (values new-form expanded)))))
    (frob form nil)))
