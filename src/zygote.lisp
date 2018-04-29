(in-package :zygote)


(defcfun zyg-listen :int
  (sun-path :string))

(defcfun zyg-accept :int
  (sock :int))

(defcfun zyg-process :pointer
  (sock :int))

(defcfun zyg-recv-string :pointer
  (sock :int))

(defcfun zyg-recv-stdio :int
  (sock :int))

(define-foreign-library libclzygote
  (t (:default "libclzygote")))
(use-foreign-library libclzygote)

(defun recv-string (sock)
  (let ((buf (zyg-recv-string sock)))
    (if (null-pointer-p buf)
        nil
        (let ((msg (foreign-string-to-lisp buf)))
          (foreign-free buf)
          msg))))

(defun child (sock)
  (zyg-recv-stdio sock)
  ;; (format *error-output* "~A ~A~%"
  ;;         (lisp-implementation-type)
  (let ((cwd (recv-string sock)))
    (sb-posix:chdir cwd))
  (loop for msg = (recv-string sock)
     while msg
     do (eval (read-from-string msg)))
  (sb-ext:exit))

(defun handle (csock)
  (finish-output *standard-output*)
  (finish-output *error-output*)
  (let ((pid (sb-posix:fork)))
    (if (zerop pid)
        ;; child
        (if (zerop (sb-posix:fork))
            (child csock) ; grandchild
            (sb-ext:exit))
        ;; parent
        (progn
          (sb-posix:waitpid pid 0) ; reap
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
