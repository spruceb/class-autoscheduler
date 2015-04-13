(define-condition generator-end (error) ())

(defun list-to-generator (list)
  (lambda ()
    (if list
	(pop list)
	(error 'generator-end))))

(defun next (generator)
  (funcall generator))

(defun gen-map (func generator)
  (lambda ()
    (funcall func (next generator))))

(defun gen-filter (func generator)
  (lambda ()
    (let ((x (next generator)))
      (loop while (not (funcall func x)) do
	(setf x (next generator)))
      x)))

(defmacro for ((symbol in generator) &rest expressions)
  `(let ((,symbol nil))
     (loop
       (handler-case
	   (setf ,symbol (next ,generator))
	 (generator-end () (return)))
       (progn ,@expressions))))
     

(defun generator-to-list (generator)
  (let ((result nil))
    (for (i in generator)
	 (push i result))
    (reverse result)))
	  
(defun cartesian-product-generator (&rest lists)
  (let ((indicies (loop for i from 1 to (length lists) collecting 0))
	(carry 0)
	(cycled nil))
    (lambda ()
      (if (and cycled (every (lambda (x) (= x 0)) indicies))
	  (error 'generator-end)
	  (let ((result (loop for i from 0 to (1- (length lists))
			  collecting (nth (nth i indicies) (nth i lists)))))
	    (incf (first (last indicies)))
	    (loop for i from (1- (length lists)) downto 0 do
		 (progn
		   (when (/= carry 0)
		     (incf (nth i indicies))
		     (setf carry 0))
		   (when (>= (nth i indicies) (length (nth i lists)))
		     (setf (nth i indicies) 0)
		     (setf carry 1))))
	    (setf cycled t)
	    result)))))
