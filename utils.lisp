(ql:quickload :cl-ppcre)
(ql:quickload :drakma)

(defun url-encode (url-string)
  (cl-ppcre:regex-replace-all "\\+" (drakma:url-encode url-string :utf-8) "%20"))

(defun intersperse (item list)
  (rest (mapcan (lambda (x) (list item x)) list)))

(defun valid-funcall-p (list)
  (if (equal (type-of (first list)) 'symbol)
      (fboundp (first list))
      nil))

(defun pairs (list)
  (loop for (elem . rest) on list
        nconc (loop for elem-2 in rest collect (list elem elem-2))))

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
