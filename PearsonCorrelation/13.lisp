(defun standard-deviation (sequence)
  (test-variables (sequence :numseq))
  (let ((mean (mean sequence))
        (n (length sequence)))
    (sqrt (/ (reduce #'+ (map 'list #'(lambda (x) (square (- mean x))) sequence))
             (1- n)))))


(defun sd (sequence)
  (test-variables (sequence :numseq))
  (let ((mean (mean sequence))
        (n (length sequence)))
    (sqrt (/ (reduce #'+ (map 'list #'(lambda (x) (square (- mean x))) sequence))
             (1- n)))))
			 
			 
			 # https://stackoverflow.com/questions/39438755/learning-lisp-defining-a-stdev-function