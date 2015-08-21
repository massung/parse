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
   #:.get
   #:.put
   #:.opt
   #:.satisfy

   ;; lisp-style combinator macros
   #:.let
   #:.let*
   #:.prog1
   #:.progn

   ;; parse combinators
   #:.modify
   #:.push
   #:.pop
   #:.any
   #:.eof
   #:.is
   #:.is-not
   #:.one-of
   #:.none-of
   #:.ignore
   #:.maybe
   #:.many
   #:.many1
   #:.many-until
   #:.sep-by
   #:.sep-by1
   #:.skip
   #:.between))

(in-package :parse)

;;; ----------------------------------------------------

(defstruct parse-state read-token tokens token-last data)

;;; ----------------------------------------------------

(defun parse-state-next-token (st)
  "Returns the next token in the token list as a cons pair."
  (cadr (parse-state-tokens st)))

;;; ----------------------------------------------------

(defun parse-state-token-class (st)
  "Returns the class of the current token."
  (caadr (parse-state-tokens st)))

;;; ----------------------------------------------------

(defun parse-state-token-value (st)
  "Returns the value of the current token."
  (cdadr (parse-state-tokens st)))

;;; ----------------------------------------------------

(defun parse (p next-token &key initial-state (errorp t) error-value)
  "Create a parse-state and pass it through a parse combinator."
  (let* ((token-cache (list nil))

         ;; create the initial parse state
         (st (make-parse-state :tokens token-cache
                               :token-last token-cache
                               :data initial-state)))

    ;; create a function that will read into the shared token list
    (setf (parse-state-read-token st)
          #'(lambda ()
              (multiple-value-bind (class value)
                  (funcall next-token)
                (car (setf (parse-state-token-last st)
                           (cdr (rplacd (parse-state-token-last st)
                                        (list (cons class value)))))))))

    ;; read the first token as the current token
    (funcall (parse-state-read-token st))

    ;; parse the token stream
    (multiple-value-bind (x okp)
        (funcall p st)
      (cond (okp (values x t))
            (errorp (error "Parse failure"))
            (t (values error-value nil))))))

;;; ----------------------------------------------------

(defmacro define-parser (name &body ps)
  "Create a parse combinator."
  (let ((st (gensym)))
    `(defun ,name (,st)

       ;; add a documentation string to the parser if provided
       ,(when (stringp (first ps)) (pop ps))

       ;; parse the combinators, return the final result
       (funcall (>> ,@ps) ,st))))

;;; ----------------------------------------------------

(defun >>= (p f)
  "Monadic bind combinator."
  #'(lambda (st)
      (multiple-value-bind (x new-st)
          (funcall p st)
        (when new-st
          (funcall (funcall f x) new-st)))))

;;; ----------------------------------------------------

(defun >> (&rest ps)
  "Monadic bind, ignore intermediate results. Return the last."
  #'(lambda (st)
      (let ((result nil))
        (dolist (p ps (values result st))
          (multiple-value-bind (x new-st)
              (funcall p st)
            (if new-st
                (setf result x st new-st)
              (return nil)))))))

;;; ----------------------------------------------------

(defun .opt (x p)
  "Optionally match a parse combinator or return x."
  #'(lambda (st)
      (multiple-value-bind (value new-st)
          (funcall p st)
        (if new-st
            (values value new-st)
          (values x st)))))

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

(defun .get ()
  "Always succeeds, returns the current parse state data."
  #'(lambda (st)
      (values (parse-state-data st) st)))

;;; ----------------------------------------------------

(defun .put (data)
  "Always succeeds, puts data into the parse state."
  #'(lambda (st)
      (let ((new-st (copy-parse-state st)))
        (setf (parse-state-data new-st) data)
        (values data new-st))))

;;; ----------------------------------------------------

(defun .satisfy (pred)
  "Passes the current token class to a predicate function."
  #'(lambda (st)
      (destructuring-bind (class . value)
          (let ((token (parse-state-next-token st)))
            (if token
                token
              (funcall (parse-state-read-token st))))
        (when (funcall pred class)
          (let ((new-st (copy-parse-state st)))
            (multiple-value-prog1
                (values value new-st)
              (pop (parse-state-tokens new-st))))))))

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

(defmacro .prog1 (p &body body)
  "Match the parse combinator p, then execute a body."
  (let ((x (gensym)))
    `(>>= ,p #'(lambda (,x) ,@body (.ret ,x)))))

;;; ----------------------------------------------------

(defmacro .progn (&body body)
  "Execute a body, return the final result."
  `(.ret (progn ,@body)))

;;; ----------------------------------------------------

(defun .modify (f)
  "Always succeeds, applys f with the parse state data."
  (.let (x (.get))
    (.put (funcall f x))))

;;; ----------------------------------------------------

(defun .push (x)
  "Always succeeds, assumes data is a list and pushes x onto it."
  (.let (xs (.get))
    (.put (cons x xs))))

;;; ----------------------------------------------------

(defun .pop ()
  "Always succeeds, assumes data is a list an pops it."
  (.let (xs (.get))
    (>> (.put (cdr xs))
        (.ret (car xs)))))

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
        (multiple-value-bind (x new-st)
            (funcall p st)
          (when new-st
            (return (values x new-st)))))))

;;; ----------------------------------------------------

(defun .none-of (&rest ps)
  "Ensures none of the parse combinators match the current token."
  #'(lambda (st)
      (dolist (p ps (values (parse-state-token-value st) st))
        (when (nth-value 1 (funcall p st))
          (return nil)))))

;;; ----------------------------------------------------

(defun .ignore (p)
  "Parse p, ignore the result."
  (>> p (.ret nil)))

;;; ----------------------------------------------------

(defun .maybe (p)
  "Try and parse p, ignore it if there."
  (.opt nil (>> p (.ret nil))))

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
