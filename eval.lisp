;;;
;;; eval.lisp -- reimplementation functions from src/code/eval.lisp
;;;

(in-package :sb-impl)

(defun simple-eval-progn-body* (progn-body lexenv)
  (unless (list-with-length-p progn-body)
    (let ((*print-circle* t))
      (error 'simple-program-error
             :format-control
             "~@<not a proper list in PROGN or implicit PROGN: ~2I~_~S~:>"
             :format-arguments (list progn-body))))
  (do* ((i progn-body rest-i)
        (rest-i (rest i) (rest i)))
      (nil)
    (if rest-i
        (simple-eval-in-lexenv* (first i) lexenv)
        (return (simple-eval-in-lexenv* (first i) lexenv)))))

(defun simple-eval-locally* (exp lexenv &key vars)
  (multiple-value-bind (body decls)
      (parse-body (rest exp) :doc-string-allowed nil)
    (let ((lexenv
           (let* ((sb-c:*lexenv* lexenv)
                  (sb-c::*free-funs* (make-hash-table :test 'equal))
                  (sb-c::*free-vars* (make-hash-table :test 'eq))
                  (sb-c::*undefined-warnings* nil))
              (sb-c::process-decls decls
                                  vars
                                  nil
                                  :lexenv lexenv
                                  :context :eval))))
      (simple-eval-progn-body* body lexenv))))

(defun simple-eval-in-lexenv* (original-exp lexenv)
  (declare (optimize (safety 1)))
  (incf *eval-calls*)
  (handler-bind
      ((sb-c:compiler-error
        (lambda (c)
          (if (boundp 'sb-c::*compiler-error-bailout*)
              (progn
                (signal c)
                nil)
              (invoke-restart 'sb-c::signal-error)))))
    (let ((exp (macroexpand* original-exp lexenv)))
      (handler-bind ((eval-error
                      (lambda (condition)
                        (error 'interpreted-program-error
                               :condition (encapsulated-condition condition)
                               :form exp))))
        (typecase exp
          (symbol
           (ecase (info :variable :kind exp)
             ((:special :global :constant :unknown)
              (symbol-value exp))
             (:alien
              (%simple-eval original-exp lexenv))))
          (list
           (let ((name (first exp))
                 (n-args (1- (length exp))))
             (case name
               ((function)
                (unless (= n-args 1)
                  (error "wrong number of args to FUNCTION:~% ~S" exp))
                (let ((name (second exp)))
                  (if (and (legal-fun-name-p name)
                           (not (consp (let ((sb-c:*lexenv* lexenv))
                                         (sb-c:lexenv-find name funs)))))
                      (%coerce-name-to-fun name)
                      (%simple-eval original-exp lexenv))))
               ((quote)
                (unless (= n-args 1)
                  (error "wrong number of args to QUOTE:~% ~S" exp))
                (second exp))
               (setq
                (unless (evenp n-args)
                  (error "odd number of args to SETQ:~% ~S" exp))
                (unless (zerop n-args)
                  (do ((name (cdr exp) (cddr name)))
                      ((null name)
                       (do ((args (cdr exp) (cddr args)))
                           ((null (cddr args))
                            (set (first args)
                                 (simple-eval-in-lexenv* (second args) lexenv)))
                         (set (first args)
                              (simple-eval-in-lexenv* (second args) lexenv))))
                    (let ((symbol (first name)))
                      (case (info :variable :kind symbol)
                        (:special)
                        (t (return (%simple-eval original-exp lexenv))))
                      (unless (type= (info :variable :type symbol)
                                     *universal-type*)
                        (return (%simple-eval original-exp lexenv)))))))
               ((progn)
                (simple-eval-progn-body* (rest exp) lexenv))
               ((eval-when)
                (destructuring-bind (eval-when situations &rest body) exp
                  (declare (ignore eval-when))
                  (multiple-value-bind (ct lt e)
                      (sb-c:parse-eval-when-situations situations)
                    (declare (ignore ct lt))
                    (when e
                      (simple-eval-progn-body* body lexenv)))))
               ((locally)
                (simple-eval-locally* exp lexenv))
               ((macrolet)
                (destructuring-bind (definitions &rest body)
                    (rest exp)
                  (let ((lexenv
                         (let ((sb-c:*lexenv* lexenv))
                           (sb-c::funcall-in-macrolet-lexenv
                            definitions
                            (lambda (&key funs)
                              (declare (ignore funs))
                              sb-c:*lexenv*)
                            :eval))))
                    (simple-eval-locally* `(locally ,@body) lexenv))))
               ((symbol-macrolet)
                (destructuring-bind (definitions &rest body) (rest exp)
                  (multiple-value-bind (lexenv vars)
                      (let ((sb-c:*lexenv* lexenv))
                        (sb-c::funcall-in-symbol-macrolet-lexenv
                         definitions
                         (lambda (&key vars)
                           (values sb-c:*lexenv* vars))
                         :eval))
                    (simple-eval-locally* `(locally ,@body) lexenv :vars vars))))
               ((if)
                (destructuring-bind (test then &optional else) (rest exp)
                  (eval-in-lexenv (if (eval-in-lexenv test lexenv)
                                      then
                                      else)
                                  lexenv)))
               ((let let*)
                (destructuring-bind (definitions &rest body) (rest exp)
                  (if (null definitions)
                      (simple-eval-locally* `(locally ,@body) lexenv)
                      (%simple-eval exp lexenv))))
               (t
                (if (or (member name sb-sandbox::+allowed-functions+) ;; <<--
                        (member name sb-sandbox::*defined-symbols*))  ;; <<--
                    (if (and (symbolp name) (eq (info :function :kind name) :function))
                        (collect ((args))
                                 (dolist (arg (rest exp))
                                   (args (eval-in-lexenv arg lexenv)))
                                 (apply (symbol-function name) (args)))
                        (%simple-eval exp lexenv))
                    (error "~A is not in allowed or defined symbols" name))))))
          (t
           exp))))))

(defun eval* (original-exp)
  (simple-eval-in-lexenv* original-exp (make-null-lexenv)))
