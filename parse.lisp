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
   #:parse

   ;; parse combinator macros
   #:.let*

   ;; parse combinator functions
   #:.bind
   #:.ret
   #:.fail
   #:.opt
   #:.satisfy
   #:.any
   #:.is
   #:.is-not
   #:.one-of
   #:.none-of
   #:.many
   #:.many1
   #:.skip))

(in-package :parse)

;;; ----------------------------------------------------

(defstruct (parse-state (:constructor make-parse-state (lexer token)))
  lexer token)

;;; ----------------------------------------------------

(defun parse (parse-combinator lexer string &optional source)
  "Create a parse-state and pass it through a parse combinator."
  (lex:with-lexer (state lexer string :source source)

    ;; create a function that can fetch the next token
    (flet ((next-token ()
             (lex:read-token state)))

      ;; create the initial parse state
      (let ((st (make-parse-state #'next-token (next-token))))

        ;; parse until done or something goes wrong
        (handler-case
            (funcall parse-combinator st)
          (condition (c)

            ;; who what went wrong and where
            (let ((tok (parse-state-token st)))
              (error "~a on line ~d~@[ of ~s~] near ~s"
                     c
                     (lex:token-line tok)
                     (lex:token-source tok)
                     (lex:token-lexeme tok)))))))))

;;; ----------------------------------------------------

(defun .bind (m f)
  "Monadic bind combinator."
  (lambda (st)
    (multiple-value-bind (x okp)
        (funcall m st)
      (when okp
        (funcall (funcall f x) st)))))

;;; ----------------------------------------------------

(defun _bind (m p)
  "Monadic bind, ignore intermediate result."
  (lambda (st)
    (multiple-value-bind (x okp)
        (funcall m st)
      (declare (ignore x))
      (when okp
        (funcall p st)))))

;;; ----------------------------------------------------

(defun .ret (x)
  "Convert X into a monadic value."
  (lambda (st) (values x st)))

;;; ----------------------------------------------------

(defun .fail (st)
  "Ensures that the parse combinator fails."
  (declare (ignore st)))

;;; ----------------------------------------------------

(defun .opt (p &optional value)
  "Optionally match a parse combinator or return value."
  (lambda (st)
    (multiple-value-bind (x okp)
        (funcall p st)
      (values (if okp x value) t))))

;;; ----------------------------------------------------

(defun .satisfy (pred)
  "Verify that the current token satisfies a predicate."
  (lambda (st)
    (let ((tok (parse-state-token st)))
      (when (and tok (funcall pred tok))
        (multiple-value-prog1
            (values (lex:token-value (parse-state-token st)) t)
          (let ((next-token (funcall (parse-state-lexer st))))
            (setf (parse-state-token st) next-token)))))))

;;; ----------------------------------------------------

(defun .any ()
  "T as long as there is something in the token stream."
  (.satisfy #'identity))

;;; ----------------------------------------------------

(defun .is (class)
  "Checks if the current token is of a given class."
  (.satisfy (lambda (tok)
              (eq (lex:token-class tok) class))))

;;; ----------------------------------------------------

(defun .is-not (class)
  "Ensures the next token is not of a given class."
  (.satisfy (lambda (tok)
              (not (eq (lex:token-class tok) class)))))

;;; ----------------------------------------------------

(defun .one-of (&rest classes)
  "Checks to see if the current token is one of several classes."
  (.satisfy (lambda (tok)
              (find tok classes :key 'lex:token-class))))

;;; ----------------------------------------------------

(defun .none-of (&rest classes)
  "Ensures the current token is not any of a set of classes."
  (.satisfy (lambda (tok)
              (not (find tok classes :key 'lex:token-class)))))

;;; ----------------------------------------------------

(defun .many (p)
  "Try and parse a combinator zero or more times."
  (.opt (.many1 p)))

;;; ----------------------------------------------------

(defun .many1 (p)
  "Try and parse a combinator one or more times."
  (.bind p (lambda (x)
             (.bind (.many p) (lambda (xs)
                                (.ret (cons x xs)))))))

;;; ----------------------------------------------------

(defun .skip (p)
  "Optionally skip a parse combinator one or more times."
  (.opt (_bind (.many1 p) (.ret nil))))

;;; ----------------------------------------------------

(defmacro .let* ((&rest bindings) &body body)
  "Create a series of .bind closures and execute a body."
  (if (null bindings)
      `(.ret (progn ,@body))
    (destructuring-bind ((var form) &rest rest)
        bindings
      `(.bind ,form (lambda (,var) (.let* (,@rest) ,@body))))))
