;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:dungeon-stamps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODEL
(defclass stamp ()
  ((name :accessor name :initarg :name :initform 'stamp))
  (:documentation "Base clase for stamps."))

(defclass weapon (stamp)
  ((name   :initform 'weapon)
   (damage :accessor damage :initarg  :damage :initform (+ 3 (random 8)))))

(defclass enemy (stamp)
  ((name :accessor name :initform 'enemy)
   (hp   :accessor hp   :initarg  :hp    :initform 10)))

(defclass demon (enemy)
  ((name :accessor name :initform 'demon)
   (hp   :accessor hp   :initarg  :hp    :initform (1+ (random 8)))))

(defclass skeleton (enemy)
  ((name :accessor name :initform 'skeleton)
   (hp   :accessor hp   :initarg  :hp    :initform (1+ (random 4)))))

(defclass goblin (enemy)
  ((name :accessor name :initform 'goblin)
   (hp   :accessor hp   :initarg  :hp    :initform (1+ (random 6)))))

(defclass coin (stamp)
  ((name  :initform 'coin)
   (value :accessor value :initarg  :value :initform (1+ (random 10)))))

(defclass potion (stamp)
  ((name :accessor name :initform 'potion)))

(defclass chest (stamp)
  ((name  :accessor name  :initform 'chest)))

(defclass hero (stamp)
  ((name   :accessor name   :initform 'hero)
   (coins  :accessor coins  :initarg  :coins  :initform 0)
   (hp     :accessor hp     :initarg  :hp     :initform *maxhp*)
   (weapon :accessor weapon :initarg  :weapon :initform nil)))


(defun generate-stamp ()
  (make-instance (random-elt '(coin coin coin coin
                               skeleton skeleton skeleton
                               goblin goblin goblin
                               demon
                               potion
                               chest
                               weapon weapon weapon))))


(defgeneric interact (stamp)
  (:documentation "Interact with a stamp. Returns T if the stamp should be discarded."))

(defmethod interact ((stamp weapon))
  (setf (weapon *hero*) stamp)
  T)

(defmethod interact ((stamp enemy))
  (if (weapon *hero*)
      (cond ((< (damage (weapon *hero*)) (hp stamp))
             (decf (hp stamp) (damage (weapon *hero*)))
             (setf (weapon *hero*) nil)
             nil)                       ; don't remove the enemy
            ((= (damage (weapon *hero*)) (hp stamp))
             (setf (weapon *hero*) nil)
             T)
            ((> (damage (weapon *hero*)) (hp stamp))
             (decf (damage (weapon *hero*)) (hp stamp))
             T))
      (progn (decf (hp *hero*) (hp stamp))
             T)))

(defmethod interact ((stamp coin))
  (incf (coins *hero*) (value stamp))
  T)

(defmethod interact ((stamp chest))
  (change-class stamp 'coin :name 'coin :value (1+ (random 10)))
  nil)

(defmethod interact ((stamp potion))
  (setf (hp *hero*) *maxhp*)
  T)

(defgeneric draw-stamp (stamp x y)
  (:documentation "Renders the stamp at the specified position. Specialise :after methods for
additional rendering, like health points or damage value."))

(defmethod draw-stamp ((stamp stamp) x y)
  (draw-image (vec2 (* 120 x) (* y 160))
              (name stamp)))

(defmethod draw-stamp :after ((stamp weapon) x y)
  (print-text (format nil "~D" (damage stamp)) (+ 90 (* 120 x)) (+ 10 (* y 160))))

(defmethod draw-stamp :after ((stamp enemy) x y)
  (print-text (format nil "~D" (hp stamp)) (+ 90 (* 120 x)) (+ 140 (* y 160))))

(defmethod draw-stamp :after ((stamp coin) x y)
  (print-text (format nil "~D" (value stamp)) (+ 90 (* 120 x)) (+ 10 (* y 160))))

(defmethod draw-stamp :after ((stamp hero) x y)
  (print-text (format nil "~D" (hp stamp)) (+ 90 (* 120 x)) (+ 140 (* y 160)))
  (when (weapon stamp)
    (print-text (format nil "~D" (damage (weapon stamp))) (+ 10 (* 120 x)) (+ 10 (* y 160)))))
