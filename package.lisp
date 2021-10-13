;;;; package.lisp

(defpackage #:json-lib
  (:use #:cl)
  (:export :parse :stringify :encode-string))
