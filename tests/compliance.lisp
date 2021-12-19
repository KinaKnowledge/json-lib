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
    (ignore-errors   ;; avoiding any messiness with converted values not being able to be displayed.
     (format T "~A ~A~%" (tstring) (apply #'format nil args)))))


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
	    

(defun build-parser-test-harness (&key test-directory github-repo-url show-input write-compliance-html-file)
  (let
      ((parser-tests (get-tests :test-directory test-directory :github-repo-url github-repo-url)) ;; ensure the tests are there
       (test-results (make-hash-table :test 'equal))
       (all-parse-tests nil)
       (outstream nil)
       (total-count 0))
    (when (null parser-tests)
      (error "Unable to find the tests: check repo access and directory structure."))

    
    
    (setf all-parse-tests
	  (uiop:directory-files parser-tests)
	  total-count
	  (length all-parse-tests))
    
    ;; If a filename is provided use that otherwise if the user just put T, write the default "compliance.html"
    
    (when write-compliance-html-file
      (setf outstream
	    (create-result-page (or (and (typep write-compliance-html-file 'string)
					 write-compliance-html-file)
				    "compliance.html")))
      (emit-result-table-header outstream))
    ;(format T "outstream: ~A~%" outstream)
    (loop for file in all-parse-tests 
	  for count from 1
	  with result = nil
	  with expected-to-pass = nil 
	  with fail = nil
	  with status = nil
	  with input = nil
	  with status-symbol = nil
	  with test-name = nil
	  do
	     (setf fail nil
		   result nil
		   test-name
		   (format nil "~A" (path:basename file))
	           expected-to-pass
		   (str:starts-with? "y_" test-name))
	    
	     (when (or outstream show-input)
	       (setf input
		     (ignore-errors
		      (alexandria:read-file-into-string file :external-format :utf8))))
	     
	     ;; run the test
	     (when show-input
	       (dlog "~4D/~D: ~40A: running: ~A"
		     count
		     total-count
		     test-name
		     input))
	     
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
		   (second status))
	     (when outstream
	       (emit-result-row outstream count total-count (first status) status-symbol (second status) input)))
    (when outstream
      (emit-result-table-footer outstream)
      (close-result-page outstream))
    test-results))

(defun create-result-page (output-html-file)
  "This function creates an HTML output table showing the results of compliance to the JSON Test Suite.
   Pass a file path that is in a writable location.  Be sure to close the result by calling close_result_page."
  (let
      ((outstream (open output-html-file :direction :output :if-exists :supersede :external-format :utf8)))   
      (format outstream
	      "<html><head><title>JSON-Lib Compliance to JSON Test Suite</title><style>td { padding: 5px;  } .even { background: #F3F2F2; } .odd { background: #FAFAFA; }</style></head><body>~%")
    (format outstream
	    (str:join " "
		      (list "<h3>JSON LIb Compliance to the JSON Test Suite</h3>"
			    "Generated:"
			    (tstring)
			    "<p>These tests are sourced from the <a href='https://github.com/nst/JSONTestSuite'>JSON Test Suite</a> repository."
			    "<ul>"
			    "<li>Tests that are prefixed with N indicate that the execution of these tests should result in a parse error.  Therefore"
			    "these tests are considered passing when the parser indicates an error.</li>"
			    "<li>Tests that are prefixed with Y indicate that these tests are considered passing when they successfully decode the serialized JSON.</li>"
			    "<li>Tests that are prefied with I are indeterminate, and these are not marked as pass/fail, but if they suceed are marked as OK, or ERR if"
			    "an error is encountered during their execution.</li></ul></p>"
			    "<p>The JSON-LIB library is intentionally not compliant with respect to delimiting commas: commas are optional as a delimiter. Tests"
			    "that look for compliance to delimiting commas will not pass. See README for more details.</p>"

			    )))
	    
    outstream))

(defun close-result-page (outstream)
  (progn
    (format outstream
	    "</body></html>~%")
    (close outstream)))
    
    

(defun emit-result-table-header (fstream)
  (progn
    (format fstream
	    "<table>~% <thead><tr><th>Test #</th><th>Total Tests</th><th>Test Description</th><th>Result</th><th>Output (if any)</th></thead>~%")
    (format fstream
	    " <tbody>~%")))

(defun emit-result-table-footer (fstream)
  (format fstream
	  " </tbody>~%</table>"))

(defun emit-result-row (fstream count total-count test-name result test-output input)
  (handler-case
      (format fstream
	      "  <tr ~A><td>~D</td><td>~D</td><td>~A</td>~A<td>~A</td></tr>~%"
	      (if (eq (mod count 2) 0)
		  "class='even'"
		  "class='odd'")
	      count
	      total-count
	      (format nil "<details><summary>~A</summary><code>~S</code></details>~%"
		      test-name
		      input)
	      (cond
		((equal "PASS" result)		    
		 (format nil "<td style='background: #00FF0020;'>~A</td>" result))
		((equal "OK  " result)
		 (format nil "<td style='background: #00a2f057;'>~A</td>" result))
		((equal "FAIL" result)
		 (format nil "<td style='background: #FF000020;'>~A</td>" result))
		(T
		 (format nil "<td style='background: #00a2f0a6;'>~A</td>" result)))
	      (format nil "<details><summary><code>~A</code></summary><code>~A</code></details>~%"
		      (cond
			((typep test-output 'SIMPLE-ERROR)
			 (format nil "ERROR"))
			((typep test-output 'hash-table)
			 (format nil "HASH-TABLE count: ~A" (hash-table-count test-output)))
			((typep test-output 'string)
			 (format nil "STRING length: ~A" (length test-output)))
			((typep test-output 'array)
			 (format nil "VECTOR length: ~A" (length test-output)))
			((or (eq 'BOOLEAN (type-of test-output))
			     (typep test-output 'number))
			 (format nil "~A: ~A" (type-of test-output) test-output))
			(T
			 (format nil "~A" (type-of test-output))))
		      (if (typep test-output 'SIMPLE-ERROR)
			  (format nil "ERROR: ~A" test-output)
			  (handler-case
			      (stringify test-output)
			    (error ()
			      "Unable to display")))))			  	
    (error ()
      (format fstream
	      "  <tr><td>~D</td><td>~D</td><td>~A</td>~A<td>~A</td></tr>~%"
	      count
	      total-count
	      (format nil "<details><summary>~A</summary><code>~A</code></details>~%"
		      test-name
		      input)
	      (cond
		((equal "PASS" result)		    
		 (format nil "<td style='background: #00FF0020;'>~A</td>" result))
		((equal "OK  " result)
		 (format nil "<td style='background: #00a2f057'>~A</td>" result))
		((equal "FAIL" result)
		 (format nil "<td style='background: #FF000020;'>~A</td>" result))
		(T
		 (format nil "<td style='background: #00a2f0a6;'>~A</td>" result)))
	      "-"))))


	     
		   
	  
		
	
	
