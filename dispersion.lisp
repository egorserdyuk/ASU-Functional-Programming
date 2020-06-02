(defun mean (sample)
    (/ (apply '+ sample) ; Применяется функциональный аргемунт к списку целиком, т.е. суммируется список
       (list-length sample))) ; Деление этой сумммы на длину списка

(defun square (x)
    (* x x)) ; Вычисление квадрата

(defun dispersion (sample)
    (let ((mean_data (mean sample)) ; Получаем сначала среднее значение выборки и присваеваем mean_data
        (n (list-length sample))) ; Присваеваем n длину выборки
    (/ (reduce '+ (map 'list #'(lambda (x) (square (- mean_data x))) sample)) ; #'(lambda ...) вместо (function (lambda ...)) ; map приминяет lambda к каждому элементу списка ; reduce выполняет свертку в которой участвует аккумулятор, функциональный аргумент и список
             (1- n)))) ; Делим результат на 1 - длина выборки

(defparameter x '(1 2 3 4 5.0)) ; Задаем выборку, через значение x

(print (dispersion x)) ; Вывод результата

; http://mathprofi.ru/dispersia_diskretnoi_sluchainoi_velichiny.html
; https://www.cyberforum.ru/lisp/thread1673379.html