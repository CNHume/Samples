(dolist (lisp (directory "C:\ChrisH\allegro-projects\SK\*.lisp"))
  (format t "~&Loading \"~A\"~%" (enough-namestring (truename lisp) ".lisp"))
  (load lisp))

(eval-when (compile) (use-package 'sk))

(shiftf *print-circle* t)
