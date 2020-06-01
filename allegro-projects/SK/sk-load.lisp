(defpackage sk)

(dolist (lisp (directory "D:/Documents/Sourcetree/Samples/allegro-projects/SK/Source/*.lisp"))
  (format t "~&Loading \"~A\"~%" (enough-namestring (truename lisp) ".lisp"))
  (load lisp))

(eval-when (compile) (use-package 'sk))

;;;(shiftf *print-circle* t)
