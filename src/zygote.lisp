(in-package :zygote)


(defcfun zyg-listen :int
  (sun-path :string))

(defcfun zyg-accept :int
  (sock :int))

(defcfun zyg-process :pointer
  (sock :int))


(define-foreign-library libclzygote
  (t (:default "libclzygote")))
(use-foreign-library libclzygote)

(defun child (csock)
  (let* ((buf (zyg-process csock))
         (msg (foreign-string-to-lisp buf)))
    (format t "msg: ~A~%" msg))
  (sb-ext:exit))

(defun handle (csock)
  (finish-output *standard-output*)
  (finish-output *error-output*)
  (let ((pid (sb-posix:fork)))
    (if (zerop pid)
        (child csock) ; child
        (progn      ; parent
          (print 'parent)
          (force-output *standard-output*)
          (sb-posix:close csock)))))

(defun serve (&key
                socket )
  (let* ((socket (or socket
                     (merge-pathnames ".cl-zygote.sock"
                                      (user-homedir-pathname))))
         (sock-fd (zyg-listen (namestring socket))))
    (loop
       for csock = (zyg-accept sock-fd)
       do (handle csock))))
