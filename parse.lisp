;;;; Parsing package for Common Lisp
;;;;
;;;; Copyright (c) Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(defpackage :parse
  (:use :cl :lexer)
  (:export
   #:defparser

   ;; initiate a parse
   #:parse

   ;; monadic bind operations
   #:>>=
   #:>>

   ;; monadic functions
   #:.ret
   #:.fail
   #:.opt
   #:.is

   ;; parse combinators
   #:.one-of
   #:.many
   #:.many1
   #:.sep-by
   #:.sep-by1
   #:.skip
   #:.between

   ;; parse combinator macros
   #:.let*
   #:.prog1))


(in-package :parse)

;;; ----------------------------------------------------

(defstruct (parse-state (:constructor make-parse-state (lexer token)))
  lexer token)

;;; ----------------------------------------------------

(defmacro defparser (name &body ps)
  "Create a parse combinator."
  (let ((st (gensym)))
    `(defun ,name (,st)
       ,(when (stringp (first ps)) (pop ps))
       (funcall (>> ,@ps) ,st))))

;;; ----------------------------------------------------

(defun parse (parse-combinator lexer string &optional source)
  "Create a parse-state and pass it through a parse combinator."
  (lex:with-lexer (lex-st lexer string :source source)
    (flet ((next-token ()
             (lex:read-token lex-st)))

      ;; create the parse state
      (let ((st (make-parse-state #'next-token (next-token))))

        ;; parse until done or something goes wrong
        (handler-case
            (funcall parse-combinator st)
          (condition (c)
            (let ((tok (parse-state-token st)))
              (error "~a on line ~d~@[ of ~s~]"
                     c
                     (token-line tok)
                     (token-source tok)))))))))

;;; ----------------------------------------------------

(defun >>= (p f)
  "Monadic bind combinator."
  (lambda (st)
    (multiple-value-bind (x okp)
        (funcall (if (keywordp p) (.is p) p) st)
      (when okp
        (funcall (funcall f x) st)))))

;;; ----------------------------------------------------

(defun >> (&rest ps)
  "Monadic bind, ignore intermediate results. Return the last."
  (lambda (st)
    (let ((result nil))
      (dolist (p ps (values result t))
        (multiple-value-bind (x okp)
            (funcall (if (keywordp p) (.is p) p) st)
          (if okp
              (setf result x)
            (return)))))))

;;; ----------------------------------------------------

(defun .opt (p &optional value)
  "Optionally match a parse combinator or return value."
  (lambda (st)
    (multiple-value-bind (x okp)
        (funcall (if (keywordp p) (.is p) p) st)
      (values (if okp x value) t))))

;;; ----------------------------------------------------

(defun .ret (x)
  "Convert X into a monadic value."
  (lambda (st) (values x st)))

;;; ----------------------------------------------------

(defun .fail (datum &rest arguments)
  "Ensures that the parse combinator fails."
  (lambda (st)
    (declare (ignore st))
    (apply #'error datum arguments)))

;;; ----------------------------------------------------

(defun .is (class)
  "Checks if the current token is of a given class."
  (lambda (st)
    (let ((tok (parse-state-token st)))
      (when (and tok (eq (token-class tok) class))
        (multiple-value-prog1
            (values (token-value tok) t)
          (let ((next-tok (funcall (parse-state-lexer st))))
            (setf (parse-state-token st) next-tok)))))))

;;; ----------------------------------------------------

(defun .one-of (&rest ps)
  "Checks to see if the current token is one of several classes."
  (lambda (st)
    (dolist (p ps)
      (let ((x (funcall p st)))
        (when x
          (return (values x t)))))))

;;; ----------------------------------------------------

(defun .many (p)
  "Try and parse a combinator zero or more times."
  (.opt (.many1 p)))

;;; ----------------------------------------------------

(defun .many1 (p)
  "Try and parse a combinator one or more times."
  (>>= p (lambda (x)
           (>>= (.many p) (lambda (xs)
                            (.ret (cons x xs)))))))

;;; ----------------------------------------------------

(defun .sep-by (p sep)
  "Zero or more occurances of p separated by sep."
  (.opt (.sep-by1 p sep)))

;;; ----------------------------------------------------

(defun .sep-by1 (p sep)
  "One or more occurances of p separated by sep."
  (>>= p (lambda (x)
           (>>= (.many (>> sep p)) (lambda (xs)
                                     (.ret (cons x xs)))))))

;;; ----------------------------------------------------

(defun .skip (p)
  "Optionally skip a parse combinator one or more times."
  (.opt (>> (.many1 p) (.ret nil))))

;;; ----------------------------------------------------

(defun .between (b1 b2 p)
  "Capture a combinator between guards."
  (>> b1 (>>= p (lambda (x) (>> b2 (.ret x))))))

;;; ----------------------------------------------------

(defmacro .prog1 (p &body ps)
  "Match the parse combinators in order, return the first."
  (let ((x (gensym)))
    `(>>= ,p (lambda (,x) (>> ,@ps (.ret ,x))))))

;;; ----------------------------------------------------

(defmacro .let* ((&rest bindings) &body body)
  "Create a series of >>= closures and execute a body."
  (if (null bindings)
      `(.ret (progn ,@body))
    (destructuring-bind ((var form) &rest rest)
        bindings
      `(>>= ,form (lambda (,var) (.let* (,@rest) ,@body))))))
