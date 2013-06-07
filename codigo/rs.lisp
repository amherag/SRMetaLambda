;;Packages used by sample packages below
(ql:quickload :clsql)
(ql:quickload :cl-ppcre)
(ql:quickload :cl-who)
(ql:quickload :hunchentoot)
(ql:quickload :cxml)
(ql:quickload :cl-json)
(ql:quickload :html-template)
(ql:quickload :html-encode)
(ql:quickload :ironclad)
(ql:quickload :cl-redis)
(ql:quickload :bordeaux-threads)
(ql:quickload :cl-mongo)
(ql:quickload :drakma)

(ql:system-apropos "")

(defparameter *packages*
  '(
    ((defpackage :blog (:use :cl :cl-ppcre :clsql-sys :hunchentoot :cl-who :cl-json))
     (in-package :blog)

     (defun entry (entry)
       "Function to publish a new entry to my blog's general category."
       (cl-who:str (cl-ppcre:split "," entry)))

     (defun calendar-event (new-event date)
       "Function to publish a new event to my calendar."
       (cl:concatenate (cl:remove-duplicates new-event)
		       (cl-ppcre:scan new-event date)))
       
     (defun comment (text entry)
       "Posts a comment to entry's ID."
       (clsql-sys:commit (cl-ppcre:scan "bad words" (cl:nconc entry))))

     (defun start-server (port)
       "This function starts the http server for my blog"
       (hunchentoot:start port))

     (defun like (entry)
       "Adds a like to an entry."
       (clsql-sys:commit t entry)))

    ((defpackage :nature-blog (:use :cl :cl-ppcre :clsql-sys :hunchentoot :cl-who :cl-mongo))
     (in-package :nature-blog)

     (defun entry (entry)
       "Function to publish a new entry."
       (cl-mongo:doc-id (cl-who:str (cl-ppcre:split "," entry))))

     (defun calendar-event (new-event date)
       "Function to publish a new event."
       (cl-mongo:doc-id (cl:concatenate (cl:remove-duplicates new-event)
					(cl-ppcre:scan new-event date))))

     (defun start-server (port)
       "This function starts the http server."
       (hunchentoot:start port))

     (defun like (entry)
       "Adds a like to an entry."
       (clsql-sys:commit t entry)))

    ((defpackage :wordpress-blog (:use :cl :clsql-sys :hunchentoot :cl-who :cl-mongo :cxml))
     (in-package :wordpress-blog)

     (defun entry (entry)
       "Function to publish a new entry to my blog's general category."
       (cl-mongo:doc-id
	(cl-who:str (cxml:peek-rune "," entry))))

     (defun calendar-event (new-event date)
       "Function to publish a new event to my calendar."
       (cl:concatenate (cl:remove-duplicates new-event)
		       (cxml:peek-rune new-event date)))
       
     (defun comment (text entry)
       "Posts a comment to entry's ID."
       (clsql-sys:commit (cxml:peek-rune "bad words" (cl:nconc entry))))

     (defun start-server (port)
       "This function starts the http server for my blog"
       (hunchentoot:start port))

     (defun like (entry)
       "Adds a like to an entry."
       (clsql-sys:commit t entry)))

    ((defpackage :forum (:use :cl :cl-ppcre :clsql-sys :hunchentoot :cl-who :cxml))
     (in-package :blog)
     
     (defun post (post)
       "publish post in forum."
       (clsql-sys:commit (cl:concatenate "hello" post)))
     (defun like (post)
       "Adds a like to a post."
       (clsql-sys:commit (cl-who:str post)))
     (defun http-server (port)
       "Creates the server for my forum."
       (hunchentoot:start port))
     (defun create-forum (name)
       "It creates a forum named name."
       (clsql-sys:commit (cl-ppcre:scan-to-strings name)))
     (defun private-message (message from-user to-user)
       "Sends a message to user to-user from user from-user."
       (clsql-sys:commit from-user to-user)))

    ((defpackage :video-games-forum (:use :cl :cl-ppcre :clsql-sys :cl-who :cxml :drakma))
     (in-package :blog)
     
     (defun post (post)
       "Publish post in forum."
       (clsql-sys:commit (cl:concatenate "hello" post)))
     (defun like (post)
       "Adds a like to a post."
       (clsql-sys:commit (cl-who:str post)))
     (defun http-server (port)
       "Creates the server for my forum."
       (drakma:http-request port))
     (defun create-forum (name)
       "It creates a forum named name."
       (clsql-sys:commit (cl-ppcre:scan-to-strings name)))
     (defun private-message (message from-user to-user)
       "Sends a message to user to-user from user from-user."
       (clsql-sys:commit from-user to-user)))

    ((defpackage :web-site (:use :cl :clsql-sys :hunchentoot :alexandria.0.DEV :cxml))
     (in-package :web-site)
     
     (defun http-server (port)
       "Creates the server for my web site."
       (alexandria.0.DEV:flatten port))
     
     (defun create-forum (name)
       "It creates a forum named name."
       (clsql-sys:commit (cxml:peek-rune name))))

    ((defpackage :anime-web-site (:use :cl :alexandria.0.DEV :cxml :redis))
     (in-package :anime-web-site)
     
     (defun post (post)
       "publish post in web site."
       (redis:red-del (clsql-sys:commit (cl:concatenate "hello" post))))

     (defun http-server (port)
       "Creates the server for my web site."
       (alexandria.0.DEV:flatten port))
     
     (defun create-forum (name)
       "It creates a forum named name."
       (clsql-sys:commit (cxml:peek-rune name))))

    ))




;;======PACKAGE SIMILARITY======;;

;;======;;
;;UTILITIES;;
;;======;;

(ql:quickload "cl-ppcre")
(ql:quickload "alexandria")


(defun dot-product (list1 list2)
  (apply #'+ (mapcar (lambda (elt1 elt2)
		       (* elt1 elt2)) list1 list2)))

(defun magnitude (lst)
  (sqrt (apply #'+ (mapcar (lambda (elt)
			     (expt elt 2)) lst))))




;;=================;;
;;COLLABORATIVE FILTERING;;
;;=================;;

;;PACKAGE SIMILARITY BY PACKAGE USAGE;;

(defun p-counts (p packages)
  (let ((count 0)
	(p (if (stringp p)
	       (string-upcase p)
	       (package-name p))))
    (mapcar (lambda (pack)
	      (block is-package-present
		(mapcar (lambda (function)

			  (mapcar (lambda (sym)
				    (if (symbolp sym)
					(when (string= (package-name (symbol-package sym))
						       p)
					  (incf count)
					  (return-from is-package-present)))) (alexandria:flatten (cadr (cdddr function))))
			  )
			(cddr pack))))
	    packages)
    count))

(defun p-tf (p package)
  (let ((count 0)
	(p (if (stringp p)
	       (string-upcase p)
	       (package-name p))))
    (mapcar (lambda (function)
			(mapcar (lambda (sym)
				  (if (symbolp sym)
				      (when (string= (package-name (symbol-package sym))
						     p)
					(incf count)))) (alexandria:flatten (cadr (cdddr function))))
			)
		      (cddr package))
    count))

(defun p-idf (p packages)
  (let ((c (p-counts p packages)))
    (log (/ (length packages)
	    (if (eq c 0)
	    1
	    c))
    )))

(defun p-tf-idf (p package packages)
  (* (p-tf p package)
     (p-idf p packages)))


(defun get-packages-list (packages)
  "Done."
  (mapcar (lambda (p)
	    (package-name p)) (remove-duplicates (alexandria:flatten (mapcar (lambda (package)
	    (cdar (cddr (car package)))) packages)))))

(get-packages-list *packages*)

(defun p-tf-idf-vector (package packages-list packages)
  (mapcar (lambda (p)
	    (p-tf-idf p package packages)) packages-list))

(defun p-cosine-similarity (package1 package2 packages)
  (let* ((packages-list (get-packages-list packages))
	 (tf-idf-p1 (p-tf-idf-vector package1 packages-list packages))
	 (tf-idf-p2 (p-tf-idf-vector package2 packages-list packages)))
    (/ (dot-product tf-idf-p1 tf-idf-p2)
       (* (magnitude tf-idf-p1) (magnitude tf-idf-p2)))))

(p-cosine-similarity (first *packages*)
		   (second *packages*)
		   *packages*)

;;===========================;;
;;PACKAGE SIMILARITY BY DOCUMENTATION;;
;;===========================;;

(defun counts (term documents)
  (let ((count 0))
    (map nil (lambda (document)
	    (when (cl-ppcre:scan term document)
	      (incf count))) documents)
    count))

;;(counts "hola" '("hola como estas" "aqui dice hola hola hola hola hola" "aca no dice"))

(defun tf (term document)
  (/ (length (cl-ppcre:all-matches term document)) 2))

;;(tf "hola" "hola como estas. solo queria decirte hola hola.")

(defun idf (term documents)
  (let ((c (counts term documents)))
    (log (/ (length documents)
	    (if (eq c 0)
		1
		c)
	    ))))

;;(idf "hola" '("hola como estas hola hola hola" "espero que hola estes bien" "aqui no dice hola"))

(defun tf-idf (term document documents)
  (* (tf term document)
     (idf term documents)))

(tf-idf "chango" "hola como estas chango" '("hola como estas chango" "espero que estes bien" "aqui no dice hola"))

(defun get-words-list (documents)
  "Gets a list of the words present in the documents."
  (remove-duplicates
   (alexandria:flatten (mapcar (lambda (document)
				 (cl-ppcre:split "[\\s-\\.,;:\\(\\)]+" document)) documents)) :test #'string-equal))

;;(get-words-list '("hola como estas" "espero que estes bien" "aqui no dice hola"))

(defun tf-idf-vector (document words-list documents)
  (mapcar (lambda (word)
	    (tf-idf word document documents)) words-list))

#|
(tf-idf-vector "hola como estas"
	       (get-words-list '("hola como estas" "espero que estes bien" "aqui no dice hola"))
	       '("hola como estas" "espero que estes bien" "aqui no dice hola"))
|#

(defun cosine-similarity (document1 document2 documents)
  (let* ((words-list (get-words-list documents))
	(tf-idf-doc1 (tf-idf-vector document1 words-list documents))
	(tf-idf-doc2 (tf-idf-vector document2 words-list documents)))
    (/ (dot-product tf-idf-doc1 tf-idf-doc2)
       (* (magnitude tf-idf-doc1) (magnitude tf-idf-doc2)))))

#|
(cosine-similarity "" ""
		   '("hola como estas" "espero que estes bien" "aqui no dice hola"))
|#

(defun get-documentations (packages)
  "Done."
  (mapcar (lambda (docs)
	    (apply #'concatenate 'string (mapcar (lambda (doc)
						   (concatenate 'string doc " ")) docs)))
	  (mapcar (lambda (package)
		    (mapcar (lambda (function)
			      (first (cdddr function))) (cddr package))) packages)))

;;(get-documentations *packages*)

(defun get-documentation (package)
  (apply #'concatenate 'string (mapcar (lambda (function)
					 (first (cdddr function))) (cddr package))))

;;(get-documentation (second *packages*))

#|
(cosine-similarity (first (get-documentations *packages*))
		   (first (get-documentations *packages*))
		   (get-documentations *packages*))
|#


;;=========================;;
;;PACKAGE SIMILARITY BY SOURCE CODE;;
;;=========================;;

(defun get-source-codes (packages)
  (mapcar (lambda (package)
	    (format nil "~s" package)) (mapcar (lambda (package)
	    (mapcar (lambda (function)
		      (list (first function)
			    (second function)
			    (third function)
			    (cadr (cdddr function)))) (cddr package))) packages)))

;;(get-source-codes *packages*)

(defun get-source-code (package)
  (format nil "~s" (mapcar (lambda (function)
	    (list (first function)
		  (second function)
		  (third function)
		  (cadr (cdddr function)))) (cddr package))))

;;(get-source-code (first *packages*))

#|
(cosine-similarity (first (get-source-codes *packages*))
		   (first (get-source-codes *packages*))
		   (get-source-codes *packages*))
|#

(defun similarity (package1 package2 packages)
  (list (p-cosine-similarity package1 package2 packages)
	(cosine-similarity (get-documentation package1)
			   (get-documentation package2)
			   (get-documentations packages))
	(cosine-similarity (get-source-code package1)
			   (get-source-code package2)
			   (get-source-codes packages))))

#|
(similarity (second *packages*)
	    (first *packages*)
	    *packages*)
|#

(defun average-similarity (package1 package2 packages)
  (/ (apply #'+ (similarity package1 package2 packages))3 ))

#|
(average-similarity (first *packages*)
		    (fifth *packages*)
		    *packages*)
|#


;;===============;;
;;RECOMMENDER SYSTEM;;
;;===============;;

(defun used-packages (package)
  (cdar (cddar package)))

;;(used-packages (first *packages*))

(defun dif-used-packages (package1 package2)
  (let ((p1 (used-packages package1))
	(p2 (used-packages package2)))
    (map nil (lambda (p)
	       (setf p2 (remove p p2))) p1)
    p2))

;;(used-packages (first *packages*))
;;(used-packages (second *packages*))
;;(dif-used-packages (second *packages*) (first *packages*))

(defun get-package-name (package)
  (cadar package))

;;(get-package-name (second *packages*))

(defun recommender-system (package packages)
  (format t "Similar Packages:~%~%")
  (map nil (lambda (rec)
	     (format t "  ~a  ~2$%~% " (first rec) (* (second rec) 100))
	     (format t "    Recommended Packages:~%")
	     (map nil (lambda (p)
			(format t "        * ~a~%" p)) (third rec))
	     (format t "~%"))
       (sort (mapcar (lambda (p)
		 (list (get-package-name p)
		       (average-similarity package p packages)
		       (dif-used-packages package p)))
	       (remove package packages)) #'> :key #'second))
  t)

;;==================;;
;;RECOMMENDER SYSTEM TEST;;
;;==================;;

;;===========================;;

;;(recommender-system (first *packages*) *packages*)
;;(get-package-name (fourth *packages*))

;;===========================;;
