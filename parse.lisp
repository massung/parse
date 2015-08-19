;;;; Monadic parsing package for Common Lisp
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
  (:use :cl)
  (:export
   #:parse

   ;; declare a parse combinator
   #:define-parser

   ;; monadic bind operations
   #:>>=
   #:>>

   ;; monadic functions
   #:.ret
   #:.fail
   #:.opt
   #:.satisfy

   ;; parse combinators
   #:.any
   #:.eof
   #:.is
   #:.is-not
   #:.one-of
   #:.none-of
   #:.many
   #:.many1
   #:.many-until
   #:.sep-by
   #:.sep-by1
   #:.skip
   #:.between

   ;; lisp-style combinators
   #:.let
   #:.let*
   #:.prog1
   #:.progn))


(in-package :parse)

;;; ----------------------------------------------------

(defstruct parse-state read-token token-class token-value)

;;; ----------------------------------------------------

(defun parse (p next-token &optional (errorp t) error-value)
  "Create a parse-state and pass it through a parse combinator."
  (let ((st (make-parse-state)))

    ;; create a function that will read tokens into the parser
    (setf (parse-state-read-token st)
          #'(lambda ()
              (multiple-value-bind (class value)
                  (funcall next-token)
                (setf (parse-state-token-class st) class
                      (parse-state-token-value st) value))))

    ;; read the first token as the current token
    (funcall (parse-state-read-token st))

    ;; parse the token stream
    (multiple-value-bind (x okp)
        (funcall p st)
      (let ((emptyp (null (parse-state-token-class st))))
        (if okp
            (values x t)
          (if (null errorp)
              (values error-value emptyp)
            (error "Parse error")))))))

;;; ----------------------------------------------------

(defmacro define-parser (name &body ps)
  "Create a parse combinator."
  (let ((st (gensym)))
    `(defun ,name (,st)

       ;; add a documentation string to the parser if provided
       ,(when (stringp (first ps)) (pop ps))

       ;; parse the combinators as a progn
       (funcall (>> ,@ps) ,st))))

;;; ----------------------------------------------------

(defun >>= (p f)
  "Monadic bind combinator."
  #'(lambda (st)
      (multiple-value-bind (x okp)
          (funcall p st)
        (when okp
          (funcall (funcall f x) st)))))

;;; ----------------------------------------------------

(defun >> (&rest ps)
  "Monadic bind, ignore intermediate results. Return the last."
  #'(lambda (st)
      (let ((result nil))
        (dolist (p ps (values result t))
          (multiple-value-bind (x okp)
              (funcall p st)
            (if okp
                (setf result x)
              (return nil)))))))

;;; ----------------------------------------------------

(defun .opt (x p)
  "Optionally match a parse combinator or return x."
  #'(lambda (st)
      (multiple-value-bind (value okp)
          (funcall p st)
        (values (if okp value x) t))))

;;; ----------------------------------------------------

(defun .ret (x)
  "Convert X into a monadic value."
  #'(lambda (st) (values x st)))

;;; ----------------------------------------------------

(defun .fail (datum &rest arguments)
  "Ensures that the parse combinator fails."
  #'(lambda (st)
      (declare (ignore st))
      (apply #'error datum arguments)))

;;; ----------------------------------------------------

(defun .satisfy (pred)
  "Passes the current token class to a predicate function."
  #'(lambda (st)
      (when (funcall pred (parse-state-token-class st))
        (multiple-value-prog1
            (values (parse-state-token-value st) t)
          (when (parse-state-token-class st)
            (funcall (parse-state-read-token st)))))))

;;; ----------------------------------------------------

(defun .any ()
  "Succeeds if not at the end of the token stream."
  (.satisfy #'identity))

;;; ----------------------------------------------------

(defun .eof ()
  "Succeeds if at the end of the token stream."
  (.satisfy #'null))

;;; ----------------------------------------------------

(defun .is (class)
  "Checks if the current token is of a given class."
  (.satisfy #'(lambda (c) (eql c class))))

;;; ----------------------------------------------------

(defun .is-not (class)
  "Ensures that the current token is not of a given class."
  (.satisfy #'(lambda (c) (not (eq c class)))))

;;; ----------------------------------------------------

(defun .one-of (&rest ps)
  "Checks to see if the current token matches a combinator."
  #'(lambda (st)
      (dolist (p ps)
        (multiple-value-bind (x okp)
            (funcall p st)
          (when okp
            (return (values x t)))))))

;;; ----------------------------------------------------

(defun .none-of (&rest ps)
  "Ensures none of the parse combinators match the current token."
  #'(lambda (st)
      (dolist (p ps (values (parse-state-token-value st) t))
        (when (nth-value 1 (funcall p st))
          (return)))))

;;; ----------------------------------------------------

(defun .many (p)
  "Try and parse a combinator zero or more times."
  (.opt nil (.many1 p)))

;;; ----------------------------------------------------

(defun .many1 (p)
  "Try and parse a combinator one or more times."
  (>>= p #'(lambda (x)
             (>>= (.many p) #'(lambda (xs)
                                (.ret (cons x xs)))))))

;;; ----------------------------------------------------

(defun .many-until (p term)
  "Parse zero or more combinators until a terminal is reached."
  (.one-of (>> term (.ret nil))
           (>>= p #'(lambda (x)
                      (>>= (.many-until p term)
                           #'(lambda (xs)
                               (.ret (cons x xs))))))))

;;; ----------------------------------------------------

(defun .sep-by (p sep)
  "Zero or more occurances of p separated by sep."
  (.opt nil (.sep-by1 p sep)))

;;; ----------------------------------------------------

(defun .sep-by1 (p sep)
  "One or more occurances of p separated by sep."
  (>>= p #'(lambda (x)
             (>>= (.many (>> sep p)) #'(lambda (xs)
                                         (.ret (cons x xs)))))))

;;; ----------------------------------------------------

(defun .skip (p)
  "Optionally skip a parse combinator one or more times."
  (.opt nil (>> (.many1 p) (.ret nil))))

;;; ----------------------------------------------------

(defun .between (open-guard close-guard p)
  "Capture a combinator between guards."
  (>> open-guard (>>= p #'(lambda (x) (>> close-guard (.ret x))))))

;;; ----------------------------------------------------

(defmacro .let ((var p) &body body)
  "Bind a parse combinator result to var and return the body."
  `(>>= ,p #'(lambda (,var) (declare (ignorable ,var)) ,@body)))

;;; ----------------------------------------------------

(defmacro .let* ((&rest bindings) &body body)
  "Create a series of >>= closures and execute a body."
  (if bindings
      `(.let ,(first bindings)
         (.let* ,(rest bindings)
           ,@body))
    `(progn ,@body)))

;;; ----------------------------------------------------

(defmacro .prog1 (p &body ps)
  "Match the parse combinators in order, return the first."
  (let ((x (gensym)))
    `(>>= ,p #'(lambda (,x) (>> ,@ps (.ret ,x))))))

;;; ----------------------------------------------------

(defmacro .progn (&body ps)
  "Just a synonym for >>."
  `(>> ,@ps))
