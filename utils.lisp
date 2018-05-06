;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:dungeon-stamps)

(defun random-elt (seq)
  "Return an element at random from a list"
  (nth (random (length seq)) seq))

(defun real-time-seconds ()
  "Return seconds since certain point of time"
  (/ (get-internal-real-time) internal-time-units-per-second))
