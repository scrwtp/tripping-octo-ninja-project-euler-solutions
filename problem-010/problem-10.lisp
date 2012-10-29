#|
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
|#

(defun range-inclusive (start end)
  (loop for i from start to end collect i)) 

(defun map-to-index (value)
  (- value 5))

(defun flip-prime (array index)
  (let ((elem (elt array index)))
    (setf (elt array index) (list (first elem) (not (second elem)))) ;
    array))
  
(defun set-false-prime (array index)
  (let ((elem (elt array index)))
    (setf (elt array index) (list (first elem) nil)) ;
    array))

(defun fill-candidates (array limit)
  (loop for x from 1 to (sqrt limit) do
       (loop for y from 1 to (sqrt limit) do
	    (let ((n (+ (* 4 x x) (* y y)))) 
	      (if (and 
		   (<= n limit)
		   (or (= (rem n 12) 1) (= (rem n 12) 5)))
		  (setf array (flip-prime array (map-to-index n)))))
	    (let ((n (+ (* 3 x x) (* y y)))) 
	      (if (and 
		   (<= n limit)
		   (= (rem n 12) 7))
		  (setf array (flip-prime array (map-to-index n)))))
	    (let ((n (- (* 3 x x) (* y y))))
	      (if (and 
		   (> x y)
		   (<= n limit)
		   (= (rem n 12) 11))
		  (setf array (flip-prime array (map-to-index n))))))) ; 
  array)
			
(defun sieve-candidates (array limit)
  (loop for n from 5 to (sqrt limit) do
       (if (second (aref array (map-to-index n)))
	   (loop for p from (* n n) to limit by (* n n) do 
		(setf array (set-false-prime array (map-to-index p)))))) ; 
  array)
		
(defun array->list (array)
  (loop for i below (array-dimension array 0) collect (aref array i)))

(defun sieve->list (array)
  (append (list 2 3) (mapcar #'first (remove-if (lambda (x) (let ((val (first x))) (or (= (rem val 2) 0) (= (rem val 3) 0) (not (second x))))) (array->list array)))))
			
(defun sieve-of-atkin (limit) 
  (let* 
      ((array-length (+ (- limit 5) 1))
       (array (make-array array-length :initial-contents (mapcar (lambda (x) (list x nil)) (range-inclusive 5 limit)))))
    (sieve->list (sieve-candidates (fill-candidates array limit) limit))))
	
(defun problem-10 (limit)
  (let ((sieve (sieve-of-atkin limit)))
    (reduce #'+ sieve)))
		 
(problem-10 2000000)