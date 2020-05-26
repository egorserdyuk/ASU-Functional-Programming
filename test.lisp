(defmacro remove-lists (w)
  `(cond ((null ,w) nil)
        ((atom (car ,w)) (cons (car ,w) (remove-lists (cdr ,w))))
        ((remove-lists (cdr ,w)))))

(defun show (L)
    (if (null L)
        (progn (print "Empty list") (return-from show "404"))
        (setq a (car L) b (car L))
    )
    (defparameter list1 '())
    (loop
        for item in list
        do (progn (if (not(not item))(setq list1 (cons item list1))))
    )
    (print list1) (princ "Reverse")
)

(defparameter list (remove-lists '(1 2 a () (4 5) “er” (9 (5)) “ar”)))

(show list)