;;;; JSON-Lib Compliance Testing
;;   
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


;; NOTES
;; Uses JSONTestSuite on GitHub to validate behaviors against input JSON structures.
;;

(in-package :json-lib)
(defvar *json-test-repo* "https://github.com/nst/JSONTestSuite.git")
(defvar *git-path* "/usr/bin/git")
(defvar *verbose-mode* T)


(defun tstring ()
  "Returns a standardized time stamp as a string."
    (multiple-value-bind
	  (second minute hour day month year day-of-week dst-p tz)
	(get-decoded-time)
      (declare (ignore day-of-week dst-p))
      (format nil
	      "[~d-~d-~d ~2,'0d:~2,'0d:~2,'0d GMT~@d]"
	      year
	      month
	      day
	      hour
	      minute
	      second
	      (- tz))))

(defun dlog (&rest args)
  (when *verbose-mode*
    (format T "~A ~A~%" (tstring) (apply #'format nil args))))

(defun get-tests (&key test-directory github-repo-url)
  (let*
      ((repo (or github-repo-url *json-test-repo*))       
       (test-dir (or test-directory (format nil "~AJSONTestSuite/" (uiop:getcwd)))) 
       (parser-tests (probe-file (format nil "~Atest_parsing" test-dir))))
    (if parser-tests
	(progn
	  (dlog "test directory already exists: ~A" parser-tests)
	  parser-tests)
	(progn
	  (dlog "test directory ~A not found: fetching ~A"
		test-dir
		repo)
	  (multiple-value-bind (out err rval)
	      (uiop:run-program (list *git-path*
				      "clone"
				      *json-test-repo*)
				:output :string
				:error-output :string
				:ignore-error-status T)	    	    
	    (when (not (= rval 0))
	      (dlog "git returned: ~A" rval)
	      (mapcar #'dlog (str:lines out))
	      (mapcar #'dlog (str:lines err)))
	      
	    (setf parser-tests
		  (probe-file (format nil "~Atest_parsing" test-dir)))
	    (if parser-tests
		(progn
		  (dlog "retrieved repo")
		  parser-tests)
		(progn
		  (dlog "ERROR: Unable to retrieve repo")
		  nil)))))))
	    

(defun build-parser-test-harness (&key test-directory github-repo-url show-input)
  (let
      ((parser-tests (get-tests :test-directory test-directory :github-repo-url github-repo-url)) ;; ensure the tests are there
       (test-results (make-hash-table :test 'equal))
       (all-parse-tests nil)
       (total-count 0))
    (when (null parser-tests)
      (error "Unable to find the tests: check repo access and directory structure."))

    (setf all-parse-tests
	  (uiop:directory-files parser-tests)
	  total-count
	  (length all-parse-tests))
    
    (loop for file in all-parse-tests 
	  for count from 1
	  with result = nil
	  with expected-to-pass = nil 
	  with fail = nil
	  with status = nil
	  with status-symbol = nil
	  with test-name = nil
	  do
	     (setf fail nil
		   result nil
		   test-name
		   (format nil "~A" (path:basename file))
	           expected-to-pass
		   (str:starts-with? "y_" test-name))
	    

	     ;; run the test
	     (when show-input
	       (dlog "~4D/~D: ~40A: running: ~A"
		     count
		     total-count
		     test-name
		     (ignore-errors
		      (alexandria:read-file-into-string file :external-format :utf8))))
	     (handler-case		 
		 (setf result (parse (alexandria:read-file-into-string file :external-format :utf8)))
	       (error (e)		 
		   (setf result e
			 fail T)))

	     (setf status
		   (list (path:basename file) result fail)
		   status-symbol
		   (cond
		     ((str:starts-with? "i_" test-name)
		      (progn
			(if (third status)
			    "ERR "
			    "OK  ")))
		     ((and expected-to-pass
			   (third status))
		      "FAIL")
		     ((and (not expected-to-pass)
			   (not (third status)))
		      "FAIL")		     
		     (T
		      "PASS"))
	           (gethash test-name test-results)
		   (list status-symbol
			 (format nil "~A" (first status))
			 (second status)
			 (third status)))

	     (dlog "~4D/~D: ~45A: [ ~A ] ~A"
		   count
		   total-count
		   (first status)
		   status-symbol
		   (second status)))
    test-results))
		    
	      
		   
	     
	     
		   
	  
		
	
	
