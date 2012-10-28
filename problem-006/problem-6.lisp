#|
The sum of the squares of the first ten natural numbers is,

12 + 22 + ... + 102 = 385
The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)2 = 552 = 3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025  385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
|#

(defun range-inclusive (start end)
  (loop for i from start to end collect i)) 
  
(defun sum-of-squares (values)
  (reduce #'+ (mapcar (lambda (x) (* x x)) values)))
  
(defun square-of-sum (values)
  (let ((val (reduce #'+ values)))
    (* val val)))

(defun problem-6 (values)
  (- (square-of-sum values) (sum-of-squares values)))

(problem-6 (range-inclusive 1 100))