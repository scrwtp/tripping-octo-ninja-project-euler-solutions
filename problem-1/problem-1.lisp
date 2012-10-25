#|
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
Find the sum of all the multiples of 3 or 5 below 1000.
|#

(defun range (start end)
  (loop for i from start below end collect i))

(defun problem-1 (start end) 
  (reduce #'+ (remove-if-not (lambda (x)(or 
					 (= (rem x 3) 0) 
					 (= (rem x 5) 0))) 
			     (range start end))))

(problem-1 1 1000)				 
