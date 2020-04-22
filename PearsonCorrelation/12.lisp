(defmacro square (x)
  `(* ,x ,x))
  
(defun mean (sequence)
  (/ (reduce #'+ sequence) (length sequence)))
  


(defun correlation-coefficient (points)
  (let ((xs (map 'list #'first points))
        (ys (map 'list #'second points)))
    (let ((x-bar (mean xs))
          (y-bar (mean ys)))
      (/ (reduce #'+ (mapcar #'(lambda (xi yi) (* (- xi x-bar) (- yi y-bar)))
                             xs ys))
         (sqrt (* (reduce #'+ (mapcar #'(lambda (xi) (square (- xi x-bar)))
                                      xs))
                  (reduce #'+ (mapcar #'(lambda (yi) (square (- yi y-bar)))
                                      ys))))))))
									  
(defun correlation-test-two-sample (r1 n1 r2 n2 &key (tails :both))
  (let* ((z1 (fisher-z-transform r1))
         (z2 (fisher-z-transform r2))
         (lambda (/ (- z1 z2) (sqrt (+ (/ (- n1 3)) (/ (- n2 3)))))))
    (ecase tails
      (:both (* 2 (if (<= lambda 0) (phi lambda) (- 1 (phi lambda)))))
      (:positive (- 1 (phi lambda)))
      (:negative (phi lambda)))))
         

(defun correlation-test-two-sample-on-sequences (points1 points2 &key (tails :both))
  (let ((r1 (correlation-coefficient points1))
        (n1 (length points1))
        (r2 (correlation-coefficient points2))
        (n2 (length points2)))
    (correlation-test-two-sample r1 n1 r2 n2 :tails tails)))