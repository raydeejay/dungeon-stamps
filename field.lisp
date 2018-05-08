;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:dungeon-stamps)

(defparameter *field* (make-array '(9)))
(defparameter *field-grid* nil)

(defun make-field ()
  (let ((field (make-array '(9))))
    (loop :for slot :from 0 :upto 8 :doing
       (setf (elt field slot) (generate-stamp)))
    (setf (elt field 4) *hero*)
    field))
