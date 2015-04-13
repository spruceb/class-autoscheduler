(ql:quickload :cl-base64)
(ql:quickload :drakma)
(ql:quickload :cl-json)
(load "utils.lisp")

(with-open-file (stream "keys")
  (defparameter *consumer-key* (read-line stream))
  (defparameter *consumer-secret* (read-line stream)))

(defparameter *encoded-key-secret* (cl-base64:string-to-base64-string
				    (concatenate 'string *consumer-key* ":" *consumer-secret*)))

(setq drakma:*header-stream* (open "log.txt" :direction :output :if-exists :append))
(setq drakma:*text-content-types* (cons '("application" . "json") drakma:*text-content-types*))

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
