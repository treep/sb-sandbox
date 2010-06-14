;;;
;;; repl.lisp - implement new REPL functions for src/code/toplevel.lisp
;;;

(in-package :sb-impl)

(defmacro start-new-repl (&key (eval #'eval) noprint)
 `(progn
    (setf *repl-prompt-fun*
          #'(lambda (stream)
              (fresh-line stream)
              (write-string (format nil "~A> " (package-name *package*)) stream)))
    (setf *repl-read-form-fun*
          #'(lambda (in out)
              (when *read-suppress* (setf *read-suppress* nil))
              (let* ((eof-marker (cons nil nil))
                     (form (read in nil eof-marker)))
                (if (eq form eof-marker)
                    (quit)
                  form))))
    (setf *repl-fun-generator*
          (constantly 
           #'(lambda (noprint)
               (loop
                 (unwind-protect
                     (progn
                       (scrub-control-stack)
                       (sb-thread::get-foreground)
                       (unless noprint
                         (flush-standard-output-streams)
                         (funcall *repl-prompt-fun* *standard-output*)
                         (force-output *standard-output*))
                       (let* ((form (funcall *repl-read-form-fun* *standard-input* *standard-output*))
                              (results (multiple-value-list (interactive-eval form :eval ,eval)))) ;; <<--
                         (unless noprint
                           (dolist (result results)
                             (fresh-line)
                             (prin1 result)))))
                   (disable-stepping))))))
    (define-%defun)
    ;(define-%defmacro)
    (toplevel-repl ,noprint)))
