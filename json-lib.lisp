;;;; JSON-Lib.lisp

(in-package #:json-lib)


(defconstant +left-bracket+ #\[)
(defconstant +right-bracket+ #\])
(defconstant +left-brace+ #\{)
(defconstant +right-brace+ #\})
(defconstant +space+ #\ )
(defconstant +comma+ #\,)
(defconstant +tab+ #\Tab)
(defconstant +colon+ #\:)
(defconstant +quote+ #\")
(defconstant +new-line+ #\Linefeed)
(defconstant +period+ #\.)
(defconstant +escape-char+ #\\)
(defconstant +f-char+ #\f)
(defconstant +t-char+ #\t)
(defconstant +hyphen+ #\-)
(defconstant +in-code+ 0)
(defconstant +in-comment+ 1)
(defconstant +in-quotes+ 2)
(defconstant +in-string+ 2)
(defconstant +in-qm+ 3)
(defconstant +in-number+ 4)
(defconstant +in-array+ 5)
(defconstant +in-pairs+ 6)
(defconstant +in-boolean+ 7)
(defconstant +in-key+ 8)


(defun parse (json-text &key use-keywords-for-keys)
  "Given an encoded JSON string, returns a Common Lisp structure or value.  If use-keywords-for-keys is T, then hash table keys
   will be constructed as strings." 
  (let
      ((pos 0)
       (c nil)
       (total-length (length json-text)))
    (labels
	((next-char ()
	   (when (< pos total-length)
	     (setf c (elt json-text pos))
	     (incf pos)
	     c))
	 (peek-next-char ()
	   (if (< pos total-length)
	       (elt json-text pos)
	       nil))	 
	 (raise-error (message)
	   (error (format nil "pos: ~A: ~A: ~A*"
		  pos
		  message
		  (str:substring (max 0 (- pos 25)) (min total-length (+ pos 1)) json-text))))
	 (delimiter? (ch)
	   (if (or (null ch)
		   (char= ch +space+ )
		   (char= ch +comma+ )
		   (char= ch +new-line+)		   
		   (char= ch +tab+))		  
	       T
	       nil))
	 ;; main recursive decode routine
	 (read-obj (&optional (read-mode 0) (last-c nil))
	   
	     (let
		 ((collector (make-array 0
					 :element-type T
					 :adjustable T
					 :fill-pointer 0))
		  (str-val nil)
		  (mode read-mode)
		  (rval nil)
		  (is-float? nil))	       
	       (when last-c
		 (vector-push-extend last-c collector))
	       (loop for c = (cond
			       ((or (eq mode +in-array+)
				    (eq mode +in-pairs+)
				    (eq mode +in-number+))
				(peek-next-char))
			       (T
				(next-char)))
		     do
			(cond
			  ((eq mode +in-string+)
			   (progn			    
			     (cond
			       ((and (char= c +quote+)
				     (or (and (> pos 1)
					      (not (char= (elt json-text (- pos 2)) +escape-char+)))
					 (<= pos 1)))
			       				 
				 (loop-finish))  ;; essentially return the collector
			       (T
				(vector-push-extend c collector)))))

			  ((eq mode +in-number+)
			   (cond
			     ((delimiter? c)
			      (progn
				(next-char)
				(loop-finish)))
			     ((or (char= c +right-bracket+)
				  (char= c +right-brace+))
			      (loop-finish))
			     ((digit-char-p c)				 
			      (vector-push-extend (next-char) collector))
			     ((char= c +period+)
			      (progn
				(vector-push-extend (next-char) collector)
				(setf is-float? T)))			     
			     (T
			      (raise-error "invalid char - expected digit"))))

			  ((eq mode +in-key+)
			   (let
			       ((key (read-obj +in-string+)))			     
			     (cond
			       ((and (or (typep key 'string)
					 (typep key 'number))
				     (char= (peek-next-char) +colon+))
				(progn
				  (next-char) ;; grab the colon and return the key				  
				  (setf rval key)
				  (loop-finish)))
			       
			       (T
				(raise-error "invalid key format")))))
			      			    			  
			  

			  ((eq mode +in-boolean+)
			   (progn
			     (vector-push-extend c collector)			    
			     (cond			       
			       ((< (length collector) 4)
				nil) ;; keep collecting 
			       ((> (length collector) 5)
				(raise-error "invalid boolean"))
			       (T
				(progn								 
				  (setf str-val (map 'STRING (lambda (x) x) collector))				 
				  (cond
				    ((equal str-val "false")
				     (progn				       
				       (setf rval nil)				       
				       (loop-finish)))  ;; toss collector and return the common lisp false value
				    ((equal str-val "true")
				     (progn
				       (setf rval T)
				       (loop-finish)))))))))

			  ;; in array mode, build the vector in a loop until we hit a "]".  Then return the vector.
			  ((eq mode +in-array+)
			   (progn			     			     
			     (loop for ch = (peek-next-char)
				   do
				      				    
				      (cond					
					((char= ch +right-bracket+)
					 (progn
					   (next-char)  ;; consume the ] to account for it					
					   (loop-finish)))
					((delimiter? ch)
					 (next-char))					
					(T
					 (vector-push-extend (read-obj) collector))))
			     (setf rval
				   collector)
			     (loop-finish)))

			  ;; in pairs mode (JSON object), build pairs of objects in the collector, then
			  ;; build a hash-table and return.
			     

			  ((eq mode +in-pairs+)
			   (progn						     
			     (loop for ch = (peek-next-char)
				   with pair = nil
				   with k = nil
				   with v = nil
				   do				      
				      (cond
					((delimiter? ch)					 
					 (next-char))
					((char= ch +right-brace+)
					 (progn
					   (next-char)
					   (setf rval (make-hash-table :test 'equal))
					   (loop for (key value) across collector
						 do
						    (setf (gethash key rval)
							  value))
					   (loop-finish)))
					(T										 
					 (setf k (read-obj +in-key+))
					 (when use-keywords-for-keys
					   (setf k
						 (alexandria:make-keyword (str:replace-all "_" "-" (str:upcase k)))))
					 (setf v (read-obj))					 
					 (setf pair (list k v))				
					 (vector-push-extend pair
							     collector))))
				      
			     (loop-finish)))
				      								  
			  ;; otherwise we need to collect the value and return the correct lisp type
			  
			  (T
			   (cond
			     ((delimiter? c)
			      (progn				
				(setf rval (map 'string (lambda (x) x) collector))
				(cond
				  ((equal rval "null")
				   (progn
				    (setf rval nil)
				    (loop-finish)))
				  ((> (length collector) 0)
				   (raise-error (format nil "invalid symbol: ~A" rval)))
				  ((eq 0 (length collector))
				   nil)
				  (T				   
				   (loop-finish)))))

			      
			     ;; JSON array
			     ((char= +left-bracket+ c)
			      (progn				
				(setf rval (read-obj +in-array+))				
				(loop-finish)))

			     ;; JSON Object
			     ((char= +left-brace+ c)
			      (progn
				(setf rval (read-obj +in-pairs+))
				(loop-finish)))
			     
			     ;; there are a few special cases, numbers and booleans, which we accomodate here
			     ((eq (length collector) 0)
			      (cond
				((or (digit-char-p c)
				     (char= c +hyphen+))
				 (progn						   
				   (setf rval (read-obj +in-number+ c))						 
				   (loop-finish)))
						    
				((char= c +f-char+) (progn
						      (setf rval (read-obj +in-boolean+ c))						     
						      (loop-finish)))
				((char= c +t-char+) (progn
						      (setf rval (read-obj +in-boolean+ c))
						      (loop-finish)))
				((char= c +quote+) (progn
						     (setf rval (read-obj +in-string+))
						     (loop-finish)))
				(T
				 (vector-push-extend c collector))))
			    
			    
			     ((and (eq (length collector) 3)
				   (equal "nul" (map 'string (lambda (x) x) collector))
				   (char= c #\l))
			      (progn
				(setf rval nil)
				(loop-finish)))
			     ;; finally, if we are here just add our character to our collector
			     (T
			      (vector-push-extend c collector)))))) ;; just add the character to our collector
	       ;; return the collector based on our mode
	       
	       (cond		 		  
		 
		 ((and (eq nil rval)
		       (or (eq mode +in-boolean+)
			   (eq mode +in-code+)))
		  (setf rval nil))
		 ((not (null rval)) rval)
		 ((and (eq mode +in-number+)
		       is-float?)
		  (setf rval (parse-float:parse-float (map 'string (lambda (x) x) collector))))
		 ((eq mode +in-number+)
		  (progn		    
		    (setf rval (parse-integer (map 'string (lambda (x) x) collector)))))
		 (T
		  (setf rval (map 'string (lambda (x) x) collector))))	       
	       rval)))
      
      (read-obj))))
		  
			     


	
