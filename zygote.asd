(asdf:defsystem zygote
  :description "Fast Lisp Process Startup"
  :depends-on ("cffi")
  :components ((:file "src/package")
               (:file "src/zygote")))
