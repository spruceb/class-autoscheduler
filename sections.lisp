(ql:quickload :cl-utilities)
(ql:quickload :alexandria)
(load "utils.lisp")
(load "umich-api.lisp")
(load "class-times.lisp")

(defclass-defaults section ()
  (section-number
   section-type
   session-description
   class-topic
   enrollment-total
   enrollment-capacity
   available-seats
   wait-total
   wait-capacity
   credit-hours
   name
   class-time))

(defun make-section-from-api-alist (alist)
  (macrolet ((get-value (key &optional (alist 'alist))
	       `(cdr (assoc ,key ,alist)))
	     (get-int-value (key &optional (alist 'alist))
	       `(parse-integer (get-value ,key ,alist))))
    (make-instance
     'section
     :section-number (get-value :*section-number)
     :section-type (get-value :*section-type)
     :session-description (get-value :*session-descr)
     :class-topic (get-value :*class-topic)
     :enrollment-total (get-value :*enrollment-total)
     :enrollment-capacity (get-value :*enrollment-capacity)
     :available-seats (get-value :*available-seats)
     :wait-total (get-value :*wait-total)
     :wait-capacity (get-value :*wait-capacity)
     :credit-hours (get-value :*credit-hours)
     :class-time (let ((meeting (get-value :*meeting)))
		   (if (assoc :*times meeting)
		       (make-class-time (get-value :*times meeting)
					(get-value :*days meeting))
		       (if (apply #'equal (mapcar (lambda (x)
						    (concatenate 'string
								 (get-value :*times x)
								 (get-value :*days x)))
						  meeting))
			   (make-class-time (get-value :*times (first meeting))
					    (get-value :*days (first meeting)))
			   (error "Multiple class meetings with different times/days")))))))




(defun print-section (section)
  (format t "~a ~a ~a:~a ~a:~a ~a ~a~%"
	  (name section)
	  (section-type section)
	  (floor (/ (start-time (class-time section)) 60))
	  (mod (start-time (class-time section)) 60)
	  (floor (/ (end-time (class-time section)) 60))
	  (mod (end-time (class-time section)) 60)
	  (days (class-time section))
	  (enrollment-total section)))

(defun print-sections (sections)
  (loop for section in sections do
       (print-section section))
  (format t "~%"))

(defun get-sections-from-api-call (alist)
  (mapcar 'make-section-from-api-alist (rest alist)))

(defun class-section-objects (term-code school-code subject-code catalog-number)
  (let ((section (get-sections-from-api-call
		  (class-sections term-code school-code subject-code catalog-number))))
  (mapcar
   (lambda (x) (setf (name x)
		     (concatenate 'string
				  subject-code " "
				  (write-to-string catalog-number))))
   section)
  section))

(defun group-class-sections (section-list &key accessor)
  (let ((result (make-hash-table :test 'equal)))
    (loop for section in section-list do
	 (let ((key (slot-value section accessor)))
	   (if (gethash key result)
	       (nconc (gethash key result) (list section))
	       (setf (gethash key result) (list section)))))
    result))

(defun type-grouped-class-sections-hash (&rest class-section-args)
  (group-class-sections
   (apply #'class-section-objects class-section-args)
   :accessor'section-type))

(defun type-grouped-class-sections-list (&rest class-section-args)
  (alexandria:hash-table-values
   (apply #'type-grouped-class-sections-hash class-section-args)))

(defun sections-overlap-p (&rest sections)
  (apply #'overlap-p (mapcar 'class-time sections)))

