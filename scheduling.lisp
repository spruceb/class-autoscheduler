(load "generators.lisp")
(load "sections.lisp")
(load "umich-api.lisp")
(load "utils.lisp")

(defun valid-section-combinations (classes)
  (gen-filter
   (lambda (list) (not (apply #'sections-overlap-p list)))
   (apply #'cartesian-product-generator classes)))

(defun valid-class-combinations (&rest class-args)
  (valid-section-combinations
   (apply #'append
	  (mapcar
	   (lambda (class-identifiers)
	     (apply #'type-grouped-class-sections-list class-identifiers))
	   class-args))))
