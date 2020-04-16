(defun input(ListA ListB)
    (if (or ListA ListB)
        (append (list (car ListA) (car ListB))
                (input (cdr ListA) (cdr ListB))))
    )

(print (input '(123 321 234) '(654 324 123)))