;;;; JSON-Lib.asd

(asdf:defsystem #:json-lib
  :description "Describe JSON-Lib here"
  :author "Alex Nygren"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:str #:parse-float)
  :components ((:file "package")
               (:file "json-lib")))
