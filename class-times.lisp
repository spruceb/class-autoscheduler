(ql:quickload :cl-utilities)
(load "utils.lisp")

(defclass-defaults class-time ()
  (days 
   (start-time :initform 0)
   (end-time :initform 0)))

(defmethod initialize-instance :after ((time class-time) &key)
  (with-accessors ((start-time start-time) (end-time end-time)) time
    (when (or (not (and start-time end-time)) (> start-time end-time))
      (error "Cannot have class-time with its start-time after the end-time"))))

(defun fully-contains-p (containing-time contained-time)
  (and (<= (start-time containing-time) (start-time contained-time))
       (>= (end-time containing-time) (end-time contained-time))))

(defun time-overlap-p (&rest times)
  (setf times (sort times #'< :key 'start-time))
  (or
   (not (apply #'< (mapcar 'start-time times)))
   (not (loop for i from 0 to (- (length times) 2) always
	     (<= (end-time (nth i times)) (start-time (nth (1+ i) times)))))))

(defun days-overlap-p (first second)
  (intersection (days first) (days second) :test 'equal))

(defun two-overlap-p (first second)
  (and (days-overlap-p first second)
       (time-overlap-p first second)))

(defun overlap-p (&rest times)
  (some 'identity (mapcar
		    (lambda (pair) (apply #'two-overlap-p pair))
		    (pairs times))))

(defun time-difference (first-time second-time)
  (if (time-overlap-p first-time second-time)
      0
      (if (>= (start-time second-time) (end-time first-time))
	  (- (start-time second-time) (end-time first-time))
	  (- (start-time first-time) (end-time second-time)))))

(defun time-to-int (string)
  (destructuring-bind (first second) (cl-utilities:split-sequence #\: string)
      (+ (* (parse-integer first) 60)
	 (parse-integer second :junk-allowed t)
	 (if (and (/= (parse-integer first) 12)
		  (equal (char (reverse string) 1) #\P))
	     (* 12 60)
	     0))))

(defun time-string-split (string)
  (mapcar
   (lambda (x) (string-trim " " x))
   (cl-utilities:split-sequence #\- string)))

(defun set-time-from-string (class-time string)
  (destructuring-bind (start end) (mapcar #'time-to-int (time-string-split string))
    (setf (start-time class-time) start)
    (setf (end-time class-time) end)
    class-time))

(defun get-dates-from-string (string)
  (loop for i from 0 to (- (/ (length string) 2) 1)
     collecting (subseq string (* 2 i) (+ (* 2 i) 2))))

(defun set-dates-from-string (class-time string)
  (setf (days class-time) (get-dates-from-string string))
  class-time)

(defun make-class-time (&optional time-string day-string)
  (set-time-from-string
   (set-dates-from-string
    (make-instance 'class-time)
    day-string)
   time-string))
