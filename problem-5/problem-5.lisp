#|
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
|#

(defun range-inclusive (start end)
  (loop for i from start to end collect i)) 
 
(defun check-divisors (val divisors)
  (every (lambda (x) (= (rem val x) 0)) divisors)) 
 
(defun get-smallest-divisible-by (divisors step)
  (loop for val from step by step do 
    (if (check-divisors val divisors) (return val))))

(defun problem-5 (divisors)
  (let ((m (apply #'max divisors)))
    (get-smallest-divisible-by divisors m))) 
  
(problem-5 (range-inclusive 2 20))
