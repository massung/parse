(defpackage :parse-asd
  (:use :cl :asdf))

(in-package :parse-asd)

(defsystem :parse
  :name "parse"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "Parsing package for Common Lisp."
  :serial t
  :components ((:file "parse")))
