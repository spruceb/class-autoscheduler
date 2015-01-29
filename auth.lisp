(ql:quickload "cl-base64")
(ql:quickload :drakma)
(ql:quickload :cl-json)
(ql:quickload :cl-ppcre)
(ql:quickload :cl-utilities)
(ql:quickload :alexandria)

(require 'cl-json)
(require 'cl-base64)
(require 'drakma)
(with-open-file (stream "keys")
  (defparameter *consumer-key* (read-line stream))
  (defparameter *consumer-secret* (read-line stream)))

(defparameter *encoded-key-secret* (cl-base64:string-to-base64-string
				    (concatenate 'string *consumer-key* ":" *consumer-secret*)))

(setq drakma:*header-stream* (open "log.txt" :direction :output :if-exists :append))
(setq drakma:*text-content-types* (cons '("application" . "json") drakma:*text-content-types*))

(defun url-encode (url-string)
  (cl-ppcre:regex-replace-all "\\+" (drakma:url-encode url-string :utf-8) "%20"))


(defmacro request (url (authorization-type authorization-code) &optional params-list)
  `(drakma:http-request ,url
			:additional-headers
			`(("Authorization" . ,(concatenate 'string ,authorization-type " "
							   ,authorization-code)))
			,@params-list))

(defun post-access-token-request ()
  (request "https://api-km.it.umich.edu/token" ("Basic" *encoded-key-secret*)
	   (:method :post
	     :content "grant_type=client_credentials&scope=PRODUCTION"
	     :content-type "application/x-www-form-urlencoded")))

(let ((token nil))
  (defun get-access-token ()
    (let ((json-data (cl-json:decode-json-from-string (post-access-token-request))))
      (when (or (> 100 (cdr (assoc :expires--in json-data))) (not token))
	(setf token (cdr (assoc :access--token json-data)))))
    token))

(defun intersperse (item list)
  (rest (mapcan (lambda (x) (list item x)) list)))

(defun valid-funcall-p (list)
  (if (equal (type-of (first list)) 'symbol)
      (fboundp (first list))
      nil))

(defmacro defapicall (name (&rest params) &optional rest)
  `(defun ,name (,@params)
     (caddar (cl-json:decode-json-from-string
	      (request (apply #'concatenate 'string
			      "http://api-gw.it.umich.edu/Curriculum/SOC/v1/Terms/"
			      (intersperse "/" (mapcar (lambda (x) (url-encode
								    (princ-to-string x)))
						       ,(if (valid-funcall-p rest)
							    rest
							    `(list ,@rest)))))
		       ("Bearer" (get-access-token)))))))

(defapicall terms ())

(defapicall class-search (term-code criteria)
  (term-code "Classes" "Search" criteria))

(defapicall class-details (term-code class-number)
  (term-code "Classes" class-number))

(defapicall combined-section-details-by-class-number (term-code class-number)
  (term-code "Classes" class-number "CombinedSections"))

(defapicall schools (term-code)
  (term-code "Schools"))

(defapicall subjects (term-code school-code)
  (term-code "Schools" school-code "Subjects"))

(defapicall catalog-numbers (term-code school-code subject-code)
  (term-code "Schools" school-code "Subjects" subject-code "CatalogNbrs"))

(defapicall course-description (term-code school-code subject-code catalog-number)
  (term-code "Schools" school-code "Subjects" subject-code "CatalogNbrs" catalog-number))

(defapicall class-sections (term-code school-code subject-code catalog-number)
  (term-code "Schools" school-code "Subjects" subject-code
	     "CatalogNbrs" catalog-number "Sections"))

(defapicall section-details (term-code school-code subject-code catalog-number section-number)
  (term-code "Schools" school-code "Subjects" subject-code
	     "CatalogNbrs" catalog-number "Sections" section-number))

(defapicall combined-section-details (term-code school-code subject-code
						catalog-number section-number)
  (term-code "Schools" school-code "Subjects" subject-code
	     "CatalogNbrs" catalog-number "Sections" section-number "CombinedSections"))

(defapicall class-meetings (term-code school-code subject-code
				      catalog-number section-number)
  (term-code "Schools" school-code "Subjects" subject-code
	     "CatalogNbrs" catalog-number "Sections" section-number "Meetings"))

(defapicall class-instructors (term-code school-code subject-code
					 catalog-number section-number)
  (term-code "Schools" school-code "Subjects" subject-code
	     "CatalogNbrs" catalog-number "Sections" section-number "Instructors"))

(defapicall class-textbooks (term-code school-code subject-code catalog-number section-number)
  (term-code "Schools" school-code "Subjects" subject-code
	     "CatalogNbrs" catalog-number "Sections" section-number "Textbooks"))

(defmacro defclass-defaults (name superclasses defaulted)
  `(defclass ,name ,superclasses
     (,@(loop for item in defaulted collect
	     (if (symbolp item)
		 (list item :initarg (intern (symbol-name item) "KEYWORD") :accessor item)
		 (if (find ':initarg item)
		     item
		     (let ((arg-name (first item)))
		       (append (list arg-name
				     :initarg (intern (symbol-name arg-name) "KEYWORD")
				     :accessor arg-name)
			       (rest item)))))))))


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

(defun overlap-p (first-time second-time)
  (not (or
	(<= (start-time first-time) (end-time first-time) (start-time second-time))
	(>= (end-time first-time) (start-time first-time) (end-time second-time)))))

(defun time-difference (first-time second-time)
  (if (overlap-p first-time second-time)
      0
      (if (>= (start-time second-time) (end-time first-time))
	  (- (start-time second-time) (end-time first-time))
	  (- (start-time first-time) (end-time second-time)))))

(defun set-time-from-string (class-time string)
  (let ((index 0) (next-int 0))    
    (with-accessors ((start-time start-time) (end-time end-time)) class-time      
      (loop for i from 1 to 4 do
	   (macrolet ((set-time-var (n) `(if (< i 3) (setf start-time ,n) (setf end-time ,n)))
		      (get-time-var () `(if (< i 3) start-time end-time)))
	     (multiple-value-setq (next-int index) (parse-integer string :junk-allowed t))
	     (if (equal (mod i 2) 1)
		 (progn (set-time-var (+ (get-time-var) (* 60 next-int)))
			(setf string (subseq string (1+ index))))
		 (progn (set-time-var (+ (get-time-var) next-int))
			(setf string (subseq string index))
			(when (equal (char string 0) #\P)
			  (set-time-var (+ (get-time-var) (* 12 60))))))
	     (when (equal i 2) (setf string (subseq string 5)))))))
  class-time)

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
   class-time))

(defun make-section-from-api-alist (alist)
  (macrolet ((get-value (key &optional (alist 'alist))
	       `(cdr (assoc ,key ,alist)))
	     (get-int-value (key &optional (alist 'alist))
	       `(parse-integer (get-value ,key ,alist))))
    (make-instance 'section
		   :section-number (get-int-value :*section-number)
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
				 (make-class-time (get-value :*times meeting)
						  (get-value :*days meeting))))))

(defun print-section (section)
  (format t "~a ~a ~a ~a~%"
	  (section-type section)
	  (start-time (class-time section))
	  (end-time (class-time section))
	  (enrollment-total section)))
	  

(defun get-sections-from-api-call (alist)
  (mapcar 'make-section-from-api-alist (rest alist)))

(defun class-section-objects (term-code school-code subject-code catalog-number)
  (get-sections-from-api-call
   (class-sections term-code school-code subject-code catalog-number)))

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

(defun cartesian-product-filter (&optional (filter-function 'identity) &rest lists)
  (if (<= (length lists) 1)
      (mapcar (lambda (x) (list x)) (first lists))
      (loop for item in (first lists) appending
	   (loop for permuted in
		(apply #'cartesian-product-filter filter-function (rest lists))
	      for value = (append (list item) permuted)
	      when (funcall filter-function value)
	      collect value))))

;; (defun cartesian-product-generator (&rest lists)
;;   (let ((

(defun binary-combinations (list)
  (apply #'append
	 (loop for first in list
	    for index from 1 to (1- (length list)) collect
	      (loop for second in (nthcdr index list)
		 collecting (list first second)))))

	    

(defun valid-section-combinations (classes)
  (apply #'cartesian-product-filter
	 (lambda (list)
	   (every
	    (lambda (n) (not (overlap-p (class-time (first n)) (class-time (car (last n))))))
	    (binary-combinations list)))
	 classes))
		
(defun valid-class-combinations (&rest class-args)
  (valid-section-combinations
   (apply #'append
	 (mapcar
	  (lambda (class-identifiers)
	    (apply #'type-grouped-class-sections-list class-identifiers))
	  class-args))))
  
	 
