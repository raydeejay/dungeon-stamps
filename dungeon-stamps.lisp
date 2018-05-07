;;;; dungeon-stamps.lisp

(in-package #:dungeon-stamps)

;;; "dungeon-stamps" goes here. Hacks and glory await!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GAME STATE
(defparameter *hero* nil)
(defparameter *maxhp* 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODEL
(defclass stamp ()
  ((name :accessor name :initarg :name :initform 'stamp))
  (:documentation "Base clase for stamps."))

(defgeneric interact (stamp)
  (:documentation "Interact with a stamp. Returns T if the stamp should be discarded."))

(defgeneric draw-stamp (stamp x y)
  (:documentation "Draws the stamp at the specified position."))

(defmethod draw-stamp ((stamp stamp) x y)
  (draw-image (vec2 (* 120 x) (* y 160))
              (name stamp)))


(defclass weapon (stamp)
  ((name   :accessor name   :initform :weapon)
   (damage :accessor damage :initarg  :damage :initform (+ 3 (random 8)))))

(defmethod interact ((stamp weapon))
  (setf (weapon *hero*) stamp)
  T)

(defmethod draw-stamp :after ((stamp weapon) x y)
  (print-text (format nil "~D" (damage stamp)) (+ 10 (* 120 x)) (+ 10 (* y 160))))


(defclass demon (stamp)
  ((name :accessor name :initform :demon)
   (hp   :accessor hp   :initarg  :hp    :initform (1+ (random 10)))))

(defmethod interact ((stamp demon))
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

(defmethod draw-stamp :after ((stamp demon) x y)
  (print-text (format nil "~D" (hp stamp)) (+ 90 (* 120 x)) (+ 140 (* y 160))))


(defclass coin (stamp)
  ((name  :accessor name  :initform :coin)
   (value :accessor value :initarg  :value :initform (1+ (random 10)))))

(defmethod interact ((stamp coin))
  (incf *coins* (value stamp))
  T)

(defmethod draw-stamp :after ((stamp coin) x y)
  (print-text (format nil "~D" (value stamp)) (+ 90 (* 120 x)) (+ 10 (* y 160))))


(defclass potion (stamp)
  ((name :accessor name :initform :potion)))

(defmethod interact ((stamp potion))
  (setf (hp *hero*) *maxhp*)
  T)


(defclass hero (stamp)
  ((name   :accessor name   :initform :hero)
   (coins  :accessor coins  :initarg  :coins  :initform 0)
   (hp     :accessor hp     :initarg  :hp     :initform *maxhp*)
   (weapon :accessor weapon :initarg  :weapon :initform nil)))

(defmethod draw-stamp :after ((stamp hero) x y)
  (print-text (format nil "~D" (hp stamp)) (+ 90 (* 120 x)) (+ 140 (* y 160)))
  (when (weapon stamp)
    (print-text (format nil "~D" (damage (weapon stamp))) (+ 10 (* 120 x)) (+ 10 (* y 160)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
(defparameter *canvas-width* 400)
(defparameter *canvas-height* 600)

(defparameter *cursor-position* (gamekit:vec2 0 0))

(defparameter *current-slot* 4)
(defparameter *clicked-slot* -1)

(gamekit:defgame dungeon-stamps () ()
  (:viewport-width *canvas-width*)     ; window's width
  (:viewport-height *canvas-height*)   ; window's height
  (:viewport-title "Dungeon stamps"))  ; window's title

(defun coords-to-slot (coords)
  (let ((x (x coords))
        (y (y coords)))
    (cond ((< x 120)
           (cond ((< y 160) 0)
                 ((< y 320) 3)
                 ((< y 480) 6)))
          ((< x 240)
           (cond ((< y 160) 1)
                 ((< y 320) 4)
                 ((< y 480) 7)))
          ((< x 360)
           (cond ((< y 160) 2)
                 ((< y 320) 5)
                 ((< y 480) 8))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MOVEMENT
(defparameter *valid-moves*
  `((0 . 1) (0 . 3)
    (1 . 0) (1 . 2) (1 . 4)
    (2 . 1) (2 . 5)
    (3 . 0) (3 . 4) (3 . 6)
    (4 . 1) (4 . 3) (4 . 5) (4 . 7)
    (5 . 2) (5 . 4) (5 . 8)
    (6 . 3) (6 . 7)
    (7 . 6) (7 . 4) (7 . 8)
    (8 . 7) (8 . 5)))

(defun can-move (from to)
  (find (cons from to) *valid-moves* :test #'equal))

(defun move-to-target ()
  (setf (aref *field* *clicked-slot*) *hero*)
  (setf (aref *field* *current-slot*) (generate-stamp))
  (setf *current-slot* *clicked-slot*))

(defun try-to-move ()
  (when (can-move *current-slot* *clicked-slot*)
    (when (interact (aref *field* *clicked-slot*))
      (move-to-target))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIALISATION
(defparameter *field* (make-array '(9)))
(defparameter *field-grid* nil)

(defun generate-stamp ()
  (make-instance (random-elt '(coin coin coin coin coin
                               demon demon demon demon demon demon demon demon demon demon
                               potion
                               weapon))))

(defun make-field ()
  (let ((field (make-array '(9))))
    (loop :for slot :from 0 :upto 8 :doing
       (setf (elt field slot) (generate-stamp)))
    (setf (elt field 4) (make-instance 'hero))
    field))

(defun load-resources ()
  (gamekit:register-resource-package :keyword "img/")
  ;; (gamekit:define-sound :snake-grab "snake-grab.ogg")
  (gamekit:define-image :hero "hero.png")
  (gamekit:define-image :demon "demon.png")
  (gamekit:define-image :coin "coin.png")
  (gamekit:define-image :potion "potion.png")
  (gamekit:define-image :weapon "weapon.png")
  (gamekit:define-image :empty "empty.png")
  )

(defun bind-controls ()
  (gamekit:bind-cursor (lambda (x y)
                         "When left mouse button is pressed, update snake's head position"
                         (setf *cursor-position* (vec2 x y))))
  (gamekit:bind-button :mouse-left :pressed
                       (lambda ()
                         "Try to move to another slot"
                         ;;(gamekit:play :snake-grab)
                         (setf *clicked-slot* (coords-to-slot *cursor-position*))
                         (try-to-move)))
  ;;(gamekit:bind-button :mouse-left :released (lambda () ))
  )

(defun init-game ()
  (setf *field* (make-field)
        *field-grid* (make-array '(3 3) :displaced-to *field*)
        *coins* 0
        *kills* 0
        *hero* (make-instance 'hero)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HOOKS
(defmethod gamekit:act ((app dungeon-stamps))
  ;; possibly do something here
  )


(defmethod gamekit:draw ((app dungeon-stamps))
  (gamekit:print-text (format nil "Coins: ~D / Kills: ~D / HP: ~D" *coins* *kills* (hp *hero*)) 0 500)
  ;; let's center image position properly first
  ;; (let ((head-image-position (gamekit:subt (aref *curve* 3) (gamekit:vec2 32 32))))
  ;;   ;; then draw it where it belongs
  ;;   (gamekit:draw-image head-image-position :snake-head))

  (loop :for y :from 0 :upto 2 :do
     (loop :for x :from 0 :upto 2 :do
        (draw-stamp (aref *field-grid* y x) x y))))

(defmethod gamekit:post-initialize ((app dungeon-stamps))
  (load-resources)
  (bind-controls)
  (init-game))

(defun launch ()
  (gamekit:start 'dungeon-stamps))
