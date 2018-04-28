(in-package :zygote)


(defcfun zyg-listen :int
  (sun-path :string))

(defcfun zyg-accept :int
  (sock :int))

(defcfun zyg-process :int
  (sock :int))


(define-foreign-library libclzygote
  (t (:default "libclzygote")))
(use-foreign-library libclzygote)

(defun handle (csock)
  (finish-output *standard-output*)
  (finish-output *error-output*)
  (labels ((child ()
             (zyg-process csock)
             (print 'child)
             (sb-ext:exit))
           (parent ()
             (print 'parent)
             (sb-posix:close csock)))
    (let ((pid (sb-posix:fork)))
      (if (zerop pid)
          (child)
          (parent)))))

(defun serve (&key
                socket )
  (let* ((socket (or socket
                     (merge-pathnames ".cl-zygote.sock"
                                      (user-homedir-pathname))))
         (sock-fd (zyg-listen (namestring socket))))
    (loop
       for csock = (zyg-accept sock-fd)
       do (handle csock))))
