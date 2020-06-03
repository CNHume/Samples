(defpackage sk)

(eval-when (compile load) (use-package 'sk))

(dolist (lisp (directory "D:/Documents/Sourcetree/Samples/allegro-projects/SK/Source/*.lisp"))
  (format t "~&Loading \"~A\"~%" (enough-namestring (truename lisp) ".lisp"))
  (load lisp))

;;;(shiftf *print-circle* t)
