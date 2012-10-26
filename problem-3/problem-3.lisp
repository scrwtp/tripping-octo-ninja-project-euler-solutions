#|
The prime factors of 13195 are 5, 7, 13 and 29.
What is the largest prime factor of the number 600851475143 ?
|#

(defun naive-is-prime (n)
  "inefficient, but good enough for the task"
  (= (length (loop for i from 2 to (sqrt n) when (= (rem n i) 0) collect i)) 0))

(defun find-smallest-prime-factor (n)
  "note to self: check how to include multiple when conditions and returns so that the loop can go to sqrt n and evaluate both factors"
  (case n
    (1 1)
    (otherwise (loop for i from 2 to n
		  when (and 
			(= (rem n i) 0)
			(naive-is-prime i))
		  return i))))

(defun find-greatest-prime-factor (val)
  (let ((f (find-smallest-prime-factor val)))
    (if (= val f) f (find-greatest-prime-factor (/ val f)))))
	
(defun problem-3 (val)
  (find-greatest-prime-factor val))
  
(problem-3 600851475143)
		
				
