;;;; JSON-Lib
;; A simple, relatively fast parser and encoder.
;;
;; Copyright 2021 Alex Nygren (Kina, LLC) kinaknowledge.com

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify,  merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;; IN THE SOFTWARE.


(in-package #:json-lib)

;; CONSTANTS 

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
(defconstant +forward-slash+ #\/)
(defconstant +f-char+ #\f)
(defconstant +t-char+ #\t)
(defconstant +hyphen+ #\-)
(defconstant +e+ #\e)
(defconstant +CAP-E+ #\E)
(defconstant +u+ #\u)
(defconstant +CAP-U+ #\U)
(defconstant +plus+ #\+)
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




(defun lookup-escape (c)
  (cond
    ((char= c +escape-char+)
     +escape-char+)
    ((char= c #\n)
     +new-line+)
    ((char= c #\t)
     +tab+)
    ((char= c #\r)
     #\Newline)
    ((char= c +quote+)
     +quote+)
    (T
     nil)))
    

(defun parse (json-text &key use-keywords-for-keys trace (max-depth 1000) (max-exponent-length 2))
  "Given an encoded UTF-8 JSON string, returns a Common Lisp structure or value.  
   If use-keywords-for-keys is T, then hash table keys will be constructed as keywords. 
   By default the limit is 1000 for structural depth, but this can be set with the keyword
   max-depth. Exponent representation in serialized form is limited to a length of 2 to prevent huge values
   causing slow downs and other issues in the conversion process." 
  (let
      ((pos 0)
       (c nil)
       (escape-c nil)
       (escape-mode 0)
       (depth 0)
       (total-length (length json-text)))
    (labels
	((next-char ()
	   (when (< pos total-length)
	     (setf c (elt json-text pos))
	     (if (and (< escape-mode 2)
		      (char= c +escape-char+))
		 (setf escape-mode 2)
		 (decf escape-mode))
	     (setf escape-mode (max escape-mode 0))
	     
	     (incf pos)
	     c))
	 (peek-next-char ()
	   (if (< pos total-length)
	       (elt json-text pos)
	       nil))	 
	 (add-escape-char (c collector)
	   (progn
	     (setf escape-c (lookup-escape c))
	     (when escape-c
	       (vector-push-extend escape-c collector))))
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
		  (in-unicode nil)		  
		  (exponent-present nil)
		  (is-float? nil))
	       (incf depth)
	       (when last-c
		 (vector-push-extend last-c collector))
	       (when (> depth max-depth)
		 (raise-error "recursive parse depth exceeded max-depth"))
	       (loop for c = (cond
			       ((or (eq mode +in-array+)
				    (eq mode +in-pairs+)
				    (eq mode +in-number+))
				(peek-next-char))
			       (T
				(next-char)))
		     do
			(when trace
			  (format T "~4D ~5D ~2D ~3D ~A~%"
				  pos
				  depth
				  escape-mode
				  mode
				  c)
			  (sleep 0.2))
			(cond			  
			  ((eq mode +in-string+)
			   (progn			    
			     (cond			       
				;(add-escape-char c collector))
			       ((null c)
				(raise-error "unclosed string"))
			       ;; unicode character on-way
			       ((and (= escape-mode 1)
				     (char= c +u+))				
				  (setf in-unicode
					(make-array 4 :element-type 'character :fill-pointer 0)))
			       ;; specifically not allowed
			       ((and (= escape-mode 1)
				     (char= c +cap-u+))
				(raise-error "invalid U"))			       
			       ;; other escape codes
			       ((= escape-mode 1)
				(add-escape-char c collector))
			       ((and (= escape-mode 0)
				     (char= c +quote+))
				(loop-finish))  ;; return the collector

			       ((and (= escape-mode 0)
				     (< (char-code c) 32))
				(raise-error "unescaped control-code"))
			       ;; if in-unicode and pos equals in-unicode+4, then calc the character and place
			       ;; it in the buffer
			       (in-unicode
				(if (= (length in-unicode) 4)
				    (progn
				      (vector-push-extend (code-char (parse-integer (map 'string (lambda (x) x) in-unicode)
										    :radix 16))
							  collector)
				      (setf in-unicode
					    nil))
				    (vector-push c in-unicode)))
				  			
			       ;; otherwise just push it in
			       ((= escape-mode 0)
				(vector-push-extend c collector)))))

			  ((eq mode +in-number+)
			   (cond
			     ((null c)
			      (loop-finish))
			     ((delimiter? c)
			      (progn
				(next-char)
				(loop-finish)))
			     ((or (char= c +right-bracket+)
				  (char= c +right-brace+))
			      (loop-finish))
			     ((digit-char-p c)
			      (progn
				(when (and exponent-present
					   (> (- (length collector) exponent-present) max-exponent-length))
				  (raise-error "exponent too large"))
				(vector-push-extend (next-char) collector)))
			     ((or (char= c +e+)
				  (char= c +CAP-E+))
			      (progn
				(next-char)
				(vector-push-extend +CAP-E+ collector)
				(setf exponent-present (length collector))))				
			     ((char= c +period+)
			      (progn
				(vector-push-extend (next-char) collector)
				(setf is-float? T)))
			     ((and (or (char= c +hyphen+)
				       (char= c +plus+))
				   (> (length collector) 0)
				   (or (char= +e+ (elt collector (- (length collector) 1)))
				       (char= +CAP-E+ (elt collector (- (length collector) 1)))))
			      (vector-push-extend (next-char) collector))
			     (T
			      (raise-error "invalid char - expected digit"))))

			  ((eq mode +in-key+)
			   (let
			       ((key (read-obj +in-string+)))			     
			     (cond
			       ((and (or (typep key 'string)
					 (typep key 'number))
				     (not (null (peek-next-char)))
				     (char= (peek-next-char) +colon+))
				(progn
				  (next-char) ;; grab the colon and return the key				  
				  (setf rval key)
				  (loop-finish)))
			       
			       (T
				(raise-error "invalid key format")))))
			      			    			  
			  

			  ((eq mode +in-boolean+)
			   (progn
			     (when (null c)
			       (raise-error "invalid nil"))
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
					((null ch)
					 (raise-error "invalid nil"))
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
					((null ch)
					 (raise-error "invalid nil"))
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
				  ((and (null c)
				        (= 0 (length collector)))
				   (loop-finish))
				  ((= 0 (length collector))
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
				     (char= c +hyphen+)
				     (char= c +plus+))
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
	       (handler-case
		   (cond		 		  
		     
		     ((and (eq nil rval)
			   (or (eq mode +in-boolean+)
			       (eq mode +in-code+)))
		      (setf rval nil))
		     ((not (null rval)) rval)
		     ((and (eq mode +in-number+)
			   (or is-float?
			       exponent-present))
		      (setf rval (parse-float:parse-float (map 'string (lambda (x) x) collector))))
		     ((eq mode +in-number+)
		      (progn		    
			(setf rval (parse-integer (map 'string (lambda (x) x) collector)))))
		     (T
		      (setf rval (map 'string (lambda (x) x) collector))))
		 (error ()
		   (raise-error (format nil "invalid value"))))
	       (decf depth)
	       rval)))
      
      (read-obj))))


;; Encoders ---------------------------------

;; Main entry

(defun stringify (data &key (case-encoder #'lisp-to-snakecase) unencodable-items)
  "Converts the given data structure to a stringified JSON form, suitable for serialization and other uses.
   An optional function can be passed for case encoding of native lisp keyword structures.
   If a function for unencodable-items is provided, this function will be called, and should return a JSON 
   compliant string representing the encoded items. If no value is provided for unencodable-items, the 
   JSON null value is used."
  (encode-entry data case-encoder unencodable-items))


;; The central dispatcher
;; Simple types are handled directly otherwise they have specific
;; handlers

(defun encode-entry (entry &optional (case-encoder #'lisp-to-snakecase) unencodable-items)
  (cond
    ((typep entry 'STRING)
     (encode-string entry))
    ((typep entry 'KEYWORD)
     (write-to-string (keyword-to-string entry case-encoder)))
    ((typep entry 'NUMBER)
     (write-to-string entry))    
    ((typep entry 'NULL)
     "null")
    ((typep entry 'BOOLEAN)
     "true")
    ((typep entry 'HASH-TABLE)
     (encode-hash-table entry case-encoder unencodable-items))
    ((typep entry 'LIST)
     (encode-list entry case-encoder unencodable-items))
    ((typep entry 'VECTOR)
     (encode-vector entry case-encoder unencodable-items))
    (T
     (if unencodable-items
	 (let
	     ((encoded-value (funcall unencodable-items)))
	   (if (typep encoded-value 'string)
	       encoded-value
	       (error (format nil "non-string value returned from unencodable-items: ~A"
			      (type-of encoded-value)))))
	 "null"))))


(defun lisp-to-snakecase (text)
  (str:snake-case text))

(defun snakecase-to-lisp (text)
  (str:param-case text))

(defun lisp-to-camelcase (text)
  (str:camel-case text))


(defun keyword-to-string (kw &optional (case-encoder #'lisp-to-snakecase))
 (funcall case-encoder
	  (str:downcase (format nil "~A" kw))))

(defun encode-string (text)
  (let
      ((collector (make-array (length text) :initial-element +quote+  :element-type 'character :adjustable T :fill-pointer 1))
       (cnum nil))
    (loop for c across text
	  do
	     (setf cnum
		   (char-code c))
	     (if (< cnum 32)
		 ;; if it is a special char, encode it appropriately, otherwise discard it
		 (cond
		   ((= cnum 10)
		    (progn
		      (vector-push-extend +escape-char+ collector)
		      (vector-push-extend #\n collector)))
		   ((= cnum 9)
		    (progn
		      (vector-push-extend +escape-char+ collector)
		      (vector-push-extend #\t collector)))
		   ((= cnum 12)
		    (progn
		      (vector-push-extend +escape-char+ collector)
		      (vector-push-extend #\f collector)))
		    ((= cnum 13)
		    (progn
		      (vector-push-extend +escape-char+ collector)
		      (vector-push-extend #\r collector))))		    
		    
		 (vector-push-extend c collector)))
    (vector-push-extend +quote+ collector)
    (map 'string (lambda (x) x) collector)))
		     
		    
	       

;; Container Encoding
;; Vectors and Arrays, Lists and Hash Tables


(defun encode-vector(vect &optional (case-encoder #'lisp-to-snakecase) unencodable-items)
  (let
      ((encoded-items nil))
    (setf encoded-items
	  (loop for entry across vect
		collect (encode-entry entry case-encoder unencodable-items)))
    (str:concat "["
		(str:join ", " encoded-items)
		"]")))

(defun encode-list (items &optional (case-encoder #'lisp-to-snakecase) unencodable-items)
  (let
      ((encoded-items nil))
    (setf encoded-items
	  (loop for entry in items
		collect (encode-entry entry case-encoder unencodable-items)))
    (str:concat "["
		(str:join ", " encoded-items)
		"]")))
  


(defun encode-hash-table (table &optional (case-encoder #'lisp-to-snakecase) unencodable-items)
  (let
      ((encoded-values nil))        
    (setf encoded-values
	  (loop for k being the hash-keys of table
		  using (hash-value v)
		collect
		(list (encode-entry k case-encoder unencodable-items)
		      (encode-entry v))))
    
    (str:concat "{"
		(if (> (length encoded-values) 0)
		    (str:join ", "
			      (loop for (k v) in encoded-values
				    collect (str:concat k ": " v)))
		    "")
		"}")))
   
		   
		
	
