;;;; dungeon-stamps.lisp

(in-package #:dungeon-stamps)

;;; "dungeon-stamps" goes here. Hacks and glory await!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GAME STATE
(defparameter *hero* nil)
(defparameter *maxhp* 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODEL
(defclass weapon ()
  ((name   :accessor name   :initform :weapon)
   (damage :accessor damage :initarg  :damage)))

(defclass demon ()
  ((name :accessor name :initform :demon)
   (hp   :accessor hp   :initarg  :hp)))

(defclass coin ()
  ((name  :accessor name  :initform :coin)
   (value :accessor value :initarg  :value)))

(defclass potion ()
  ((name :accessor name :initform :potion)))

(defclass poison ()
  ())

(defclass chest ()
  ())

(defclass hero ()
  ((name   :accessor name   :initform :hero)
   (coins  :accessor coins  :initarg  :coins  :initform 0)
   (hp     :accessor hp     :initarg  :hp     :initform *maxhp*)
   (weapon :accessor weapon :initarg  :weapon :initform nil)))

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

(defun try-to-move ()
  (when (can-move *current-slot* *clicked-slot*)
    (case (name (aref *field* *clicked-slot*))
      (:coin (incf *coins*))
      (:demon (incf *kills*)
              (decf *hp*))
      (:potion (setf *hp* *maxhp*)))
    (setf (aref *field* *clicked-slot*) *hero*)
    (setf (aref *field* *current-slot*) (make-instance (random-elt '(coin demon potion))))
    (setf *current-slot* *clicked-slot*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIALISATION
(defparameter *field* (make-array '(9)))
(defparameter *field-grid* nil)

(defun make-field ()
  (let ((field (make-array '(9))))
    (loop :for slot :from 0 :upto 8 :doing
       (setf (elt field slot)
             (make-instance (random-elt '(coin demon potion)))))
    (setf (elt field 4) (make-instance 'hero))
    field))

(defun load-resources ()
  (gamekit:register-resource-package :keyword "img/")
  ;; (gamekit:define-sound :snake-grab "snake-grab.ogg")
  (gamekit:define-image :hero "hero.png")
  (gamekit:define-image :demon "demon.png")
  (gamekit:define-image :coin "coin.png")
  (gamekit:define-image :potion "potion.png")
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
  (gamekit:bind-button :mouse-left :released
                       (lambda () )))

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
  (gamekit:print-text (format nil "Coins: ~D / Kills: ~D / HP: ~D" *coins* *kills* *hp*) 0 500)
  ;; let's center image position properly first
  ;; (let ((head-image-position (gamekit:subt (aref *curve* 3) (gamekit:vec2 32 32))))
  ;;   ;; then draw it where it belongs
  ;;   (gamekit:draw-image head-image-position :snake-head))

  (loop :for y :from 0 :upto 2 :do
     (loop :for x :from 0 :upto 2 :do
        (draw-image (vec2 (* 120 x) (* y 160))
                    (name (aref *field-grid* y x))))))

(defmethod gamekit:post-initialize ((app dungeon-stamps))
  (load-resources)
  (bind-controls)
  (init-game))

(defun launch ()
  (gamekit:start 'dungeon-stamps))
