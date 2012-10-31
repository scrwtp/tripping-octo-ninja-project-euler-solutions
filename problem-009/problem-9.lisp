#|
A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,

a2 + b2 = c2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
|#
  
(defun pythagorean-triple-p (a b c)
  (= (+ (* a a) (* b b)) (* c c)))  
  
(defun generate-triples (limit)
  (let ((results nil))
    (loop for c from 1 to limit do
	  (loop for b from 1 to c do
	    (loop for a from 1 to b do
		  (if (pythagorean-triple-p a b c) (push (list a b c) results)))))
    (nreverse results)))
	
(defun find-triples-that-sum-to (sum)
  (remove-if-not (lambda (x) (= (apply #'+ x) sum)) (generate-triples sum)))
  
(defun problem-9 (sum)
  (apply #'* (first (find-triples-that-sum-to sum))))

(problem-9 1000)