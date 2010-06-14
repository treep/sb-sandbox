
(asdf:defsystem :sb-sandbox
  :description "SB-SANDBOX is a REPL with limitations."
  :version     "0.0.1"
  :serial      t
  :components  ((:file "sb-sandbox")
                (:file "macroexpand")
                (:file "eval")
                (:file "defun")
                ;(:file "defmacro")
                (:file "repl")))
