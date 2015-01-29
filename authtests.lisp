(load "auth.lisp")

"""class-time testing"""

"""Checks that creating class-time with start-time after end-time causes an error"""
(assert (not (ignore-errors
    (make-instance 'class-time :start-time 10 :end-time 5)
    t)))

"""Checks the validity of fully-contains-p"""
(assert (fully-contains-p (make-instance 'class-time :start-time 1 :end-time 20)
			  (make-instance 'class-time :start-time 5 :end-time 10)))

(assert (fully-contains-p (make-instance 'class-time :start-time 1 :end-time 20)
			  (make-instance 'class-time :start-time 1 :end-time 20)))

(assert (fully-contains-p (make-instance 'class-time :start-time 1 :end-time 20)
			  (make-instance 'class-time :start-time 1 :end-time 10)))

(assert (fully-contains-p (make-instance 'class-time :start-time 1 :end-time 20)
			  (make-instance 'class-time :start-time 5 :end-time 20)))

(assert (not (fully-contains-p (make-instance 'class-time :start-time 5 :end-time 10)
			       (make-instance 'class-time :start-time 1 :end-time 20))))

(assert (not (fully-contains-p (make-instance 'class-time :start-time 5 :end-time 10)
			       (make-instance 'class-time :start-time 1 :end-time 7))))

(assert (not (fully-contains-p (make-instance 'class-time :start-time 5 :end-time 10)
			       (make-instance 'class-time :start-time 7 :end-time 20))))

"""Checks the validity of overlap-p"""
(assert (overlap-p (make-instance 'class-time :start-time 5 :end-time 10)
		   (make-instance 'class-time :start-time 7 :end-time 20)))

(assert (overlap-p (make-instance 'class-time :start-time 7 :end-time 20)
		   (make-instance 'class-time :start-time 5 :end-time 10)))

(assert (overlap-p (make-instance 'class-time :start-time 1 :end-time 20)
			(make-instance 'class-time :start-time 10 :end-time 15)))

(assert (overlap-p (make-instance 'class-time :start-time 10 :end-time 15)
			(make-instance 'class-time :start-time 1 :end-time 20)))

(assert (not (overlap-p (make-instance 'class-time :start-time 1 :end-time 5)
			(make-instance 'class-time :start-time 10 :end-time 15))))

(assert (not (overlap-p (make-instance 'class-time :start-time 10 :end-time 15)
			(make-instance 'class-time :start-time 1 :end-time 5))))

"""Check validity of time-difference"""

(assert (equal (time-difference (make-instance 'class-time :start-time 1 :end-time 10)
				(make-instance 'class-time :start-time 15 :end-time 20))
	       5))

(assert (equal (time-difference (make-instance 'class-time :start-time 15 :end-time 20)
				(make-instance 'class-time :start-time 1 :end-time 10))
	       5))

(assert (equal (time-difference (make-instance 'class-time :start-time 1 :end-time 15)
				(make-instance 'class-time :start-time 15 :end-time 20))
	       0))

(assert (equal (time-difference (make-instance 'class-time :start-time 15 :end-time 20)
				(make-instance 'class-time :start-time 1 :end-time 15))
	       0))

(assert (equal (time-difference (make-instance 'class-time :start-time 1 :end-time 15)
				(make-instance 'class-time :start-time 10 :end-time 20))
	       0))

(assert (equal (time-difference (make-instance 'class-time :start-time 10 :end-time 20)
				(make-instance 'class-time :start-time 1 :end-time 15))
	       0))
